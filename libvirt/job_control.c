/*
 * job_control.c Core implementation of job control
 *
 * Copyright (C) 2014 Tucker DiNapoli
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 * Author: Tucker DiNapoli
 */

#include <config.h>

#include <stdlib.h>
/*

{
    // some variables declaration

    if (!(vm = qemuDomObjFromDomain(dom)))
        return -1;

    // some preparation here

    if (qemuDomainObjBeginJob(driver, vm, QEMU_JOB_MODIFY) < 0)
        goto cleanup;

    if (!virDomainObjIsActive(vm)) {
        virReportError(VIR_ERR_OPERATION_INVALID,
                       "%s", _("domain is not running"));
        goto endjob;
    }

    // Some actual work here

    ret = 0;

endjob:
    if (!qemuDomainObjEndJob(driver, vm))
        vm = NULL;

cleanup:
    if (vm)
        virObjectUnlock(vm);
    // More of cleanup work here
    return ret;
}
//rough idea
static int
jobWaitForCompletion(virDriverPtr driver, virDomainObjPtr vm,
                     struct timespec *poll_interval,bool abort_on_error)
{
    //dependent on the domain
    virDomainObjPrivatePtr priv = vm->privateData;
    //domain specific setup code

    priv->job.info.type = VIR_DOMAIN_JOB_UNBOUNDED;

    while (priv->job.info.type == VIR_DOMAIN_JOB_UNBOUNDED) {
     //cancel migration if disk I/O error is emitted while migrating
        if (abort_on_error &&
            virDomainObjGetState(vm, &pauseReason) == VIR_DOMAIN_PAUSED &&
            pauseReason == VIR_DOMAIN_PAUSED_IOERROR)
            goto cancel;

        if (qemuMigrationUpdateJobStatus(driver, vm, job, asyncJob) < 0)
            goto cleanup;

        if (dconn && virConnectIsAlive(dconn) <= 0) {
            virReportError(VIR_ERR_OPERATION_FAILED, "%s",
                           _("Lost connection to destination host"));
            goto cleanup;
        }

        virObjectUnlock(vm);

        nanosleep(&ts, NULL);

        virObjectLock(vm);
    }

 cleanup:
    if (priv->job.info.type == VIR_DOMAIN_JOB_COMPLETED)
        return 0;
    else
        return -1;

 cancel:
    if (virDomainObjIsActive(vm)) {
        if (qemuDomainObjEnterMonitorAsync(driver, vm,
                                           priv->job.asyncJob) == 0) {
            qemuMonitorMigrateCancel(priv->mon);
            qemuDomainObjExitMonitor(driver, vm);
        }
    }

    priv->job.info.type = VIR_DOMAIN_JOB_FAILED;
    virReportError(VIR_ERR_OPERATION_FAILED,
                   _("%s: %s"), job, _("failed due to I/O error"));
    return -1;
}
 */


static void
qemuDomainObjSaveJob(virQEMUDriverPtr driver, virDomainObjPtr obj)
{
    virQEMUDriverConfigPtr cfg = virQEMUDriverGetConfig(driver);

    if (virDomainObjIsActive(obj)) {
        if (virDomainSaveStatus(driver->xmlopt, cfg->stateDir, obj) < 0)
            VIR_WARN("Failed to save status on vm %s", obj->def->name);
    }

    virObjectUnref(cfg);
}

void
qemuDomainObjSetJobPhase(virQEMUDriverPtr driver,
                         virDomainObjPtr obj,
                         int phase)
{
    qemuDomainObjPrivatePtr priv = obj->privateData;
    unsigned long long me = virThreadSelfID();

    if (!priv->job.asyncJob)
        return;

    VIR_DEBUG("Setting '%s' phase to '%s'",
              qemuDomainAsyncJobTypeToString(priv->job.asyncJob),
              qemuDomainAsyncJobPhaseToString(priv->job.asyncJob, phase));

    if (priv->job.asyncOwner && me != priv->job.asyncOwner) {
        VIR_WARN("'%s' async job is owned by thread %llu",
                 qemuDomainAsyncJobTypeToString(priv->job.asyncJob),
                 priv->job.asyncOwner);
    }

    priv->job.phase = phase;
    priv->job.asyncOwner = me;
    qemuDomainObjSaveJob(driver, obj);
}

void
qemuDomainObjSetAsyncJobMask(virDomainObjPtr obj,
                             unsigned long long allowedJobs)
{
    qemuDomainObjPrivatePtr priv = obj->privateData;

    if (!priv->job.asyncJob)
        return;

    priv->job.mask = allowedJobs | JOB_MASK(QEMU_JOB_DESTROY);
}

void
qemuDomainObjDiscardAsyncJob(virQEMUDriverPtr driver, virDomainObjPtr obj)
{
    qemuDomainObjPrivatePtr priv = obj->privateData;

    if (priv->job.active == QEMU_JOB_ASYNC_NESTED)
        qemuDomainObjResetJob(priv);
    qemuDomainObjResetAsyncJob(priv);
    qemuDomainObjSaveJob(driver, obj);
}

void
qemuDomainObjReleaseAsyncJob(virDomainObjPtr obj)
{
    qemuDomainObjPrivatePtr priv = obj->privateData;

    VIR_DEBUG("Releasing ownership of '%s' async job",
              qemuDomainAsyncJobTypeToString(priv->job.asyncJob));

    if (priv->job.asyncOwner != virThreadSelfID()) {
        VIR_WARN("'%s' async job is owned by thread %llu",
                 qemuDomainAsyncJobTypeToString(priv->job.asyncJob),
                 priv->job.asyncOwner);
    }
    priv->job.asyncOwner = 0;
}

static bool
qemuDomainNestedJobAllowed(qemuDomainObjPrivatePtr priv, enum qemuDomainJob job)
{
    return !priv->job.asyncJob || (priv->job.mask & JOB_MASK(job)) != 0;
}

bool
qemuDomainJobAllowed(qemuDomainObjPrivatePtr priv, enum qemuDomainJob job)
{
    return !priv->job.active && qemuDomainNestedJobAllowed(priv, job);
}

/* Give up waiting for mutex after 30 seconds */
#define QEMU_JOB_WAIT_TIME (1000ull * 30)

/*
 * obj must be locked before calling
 */
static int ATTRIBUTE_NONNULL(1)
qemuDomainObjBeginJobInternal(virQEMUDriverPtr driver,
                              virDomainObjPtr obj,
                              enum qemuDomainJob job,
                              enum qemuDomainAsyncJob asyncJob)
{
    qemuDomainObjPrivatePtr priv = obj->privateData;
    unsigned long long now;
    unsigned long long then;
    bool nested = job == QEMU_JOB_ASYNC_NESTED;
    virQEMUDriverConfigPtr cfg = virQEMUDriverGetConfig(driver);

    VIR_DEBUG("Starting %s: %s (async=%s vm=%p name=%s)",
              job == QEMU_JOB_ASYNC ? "async job" : "job",
              qemuDomainJobTypeToString(job),
              qemuDomainAsyncJobTypeToString(priv->job.asyncJob),
              obj, obj->def->name);

    priv->jobs_queued++;

    if (virTimeMillisNow(&now) < 0) {
        virObjectUnref(cfg);
        return -1;
    }

    then = now + QEMU_JOB_WAIT_TIME;

    virObjectRef(obj);

 retry:
    if (cfg->maxQueuedJobs &&
        priv->jobs_queued > cfg->maxQueuedJobs) {
        goto error;
    }

    while (!nested && !qemuDomainNestedJobAllowed(priv, job)) {
        VIR_DEBUG("Waiting for async job (vm=%p name=%s)", obj, obj->def->name);
        if (virCondWaitUntil(&priv->job.asyncCond, &obj->parent.lock, then) < 0)
            goto error;
    }

    while (priv->job.active) {
        VIR_DEBUG("Waiting for job (vm=%p name=%s)", obj, obj->def->name);
        if (virCondWaitUntil(&priv->job.cond, &obj->parent.lock, then) < 0)
            goto error;
    }

    /* No job is active but a new async job could have been started while obj
     * was unlocked, so we need to recheck it. */
    if (!nested && !qemuDomainNestedJobAllowed(priv, job))
        goto retry;

    qemuDomainObjResetJob(priv);

    if (job != QEMU_JOB_ASYNC) {
        VIR_DEBUG("Started job: %s (async=%s vm=%p name=%s)",
                   qemuDomainJobTypeToString(job),
                  qemuDomainAsyncJobTypeToString(priv->job.asyncJob),
                  obj, obj->def->name);
        priv->job.active = job;
        priv->job.owner = virThreadSelfID();
    } else {
        VIR_DEBUG("Started async job: %s (vm=%p name=%s)",
                  qemuDomainAsyncJobTypeToString(asyncJob),
                  obj, obj->def->name);
        qemuDomainObjResetAsyncJob(priv);
        priv->job.asyncJob = asyncJob;
        priv->job.asyncOwner = virThreadSelfID();
        priv->job.start = now;
    }

    if (qemuDomainTrackJob(job))
        qemuDomainObjSaveJob(driver, obj);

    virObjectUnref(cfg);
    return 0;

 error:
    VIR_WARN("Cannot start job (%s, %s) for domain %s;"
             " current job is (%s, %s) owned by (%llu, %llu)",
             qemuDomainJobTypeToString(job),
             qemuDomainAsyncJobTypeToString(asyncJob),
             obj->def->name,
             qemuDomainJobTypeToString(priv->job.active),
             qemuDomainAsyncJobTypeToString(priv->job.asyncJob),
             priv->job.owner, priv->job.asyncOwner);

    if (errno == ETIMEDOUT)
        virReportError(VIR_ERR_OPERATION_TIMEOUT,
                       "%s", _("cannot acquire state change lock"));
    else if (cfg->maxQueuedJobs &&
             priv->jobs_queued > cfg->maxQueuedJobs)
        virReportError(VIR_ERR_OPERATION_FAILED,
                       "%s", _("cannot acquire state change lock "
                               "due to max_queued limit"));
    else
        virReportSystemError(errno,
                             "%s", _("cannot acquire job mutex"));
    priv->jobs_queued--;
    virObjectUnref(obj);
    virObjectUnref(cfg);
    return -1;
}

/*
 * obj must be locked before calling
 *
 * This must be called by anything that will change the VM state
 * in any way, or anything that will use the QEMU monitor.
 *
 * Upon successful return, the object will have its ref count increased,
 * successful calls must be followed by EndJob eventually
 */
int qemuDomainObjBeginJob(virQEMUDriverPtr driver,
                          virDomainObjPtr obj,
                          enum qemuDomainJob job)
{
    return qemuDomainObjBeginJobInternal(driver, obj, job,
                                         QEMU_ASYNC_JOB_NONE);
}

int qemuDomainObjBeginAsyncJob(virQEMUDriverPtr driver,
                               virDomainObjPtr obj,
                               enum qemuDomainAsyncJob asyncJob)
{
    return qemuDomainObjBeginJobInternal(driver, obj, QEMU_JOB_ASYNC,
                                         asyncJob);
}

int
qemuDomainObjBeginNestedJob(virQEMUDriverPtr driver,
                            virDomainObjPtr obj,
                            enum qemuDomainAsyncJob asyncJob)
{
    qemuDomainObjPrivatePtr priv = obj->privateData;

    if (asyncJob != priv->job.asyncJob) {
        virReportError(VIR_ERR_INTERNAL_ERROR,
                       _("unexpected async job %d"), asyncJob);
        return -1;
    }

    if (priv->job.asyncOwner != virThreadSelfID()) {
        VIR_WARN("This thread doesn't seem to be the async job owner: %llu",
                 priv->job.asyncOwner);
    }

    return qemuDomainObjBeginJobInternal(driver, obj,
                                         QEMU_JOB_ASYNC_NESTED,
                                         QEMU_ASYNC_JOB_NONE);
}


/*
 * obj must be locked before calling
 *
 * To be called after completing the work associated with the
 * earlier qemuDomainBeginJob() call
 *
 * Returns true if @obj was still referenced, false if it was
 * disposed of.
 */
bool qemuDomainObjEndJob(virQEMUDriverPtr driver, virDomainObjPtr obj)
{
    qemuDomainObjPrivatePtr priv = obj->privateData;
    enum qemuDomainJob job = priv->job.active;

    priv->jobs_queued--;

    VIR_DEBUG("Stopping job: %s (async=%s vm=%p name=%s)",
              qemuDomainJobTypeToString(job),
              qemuDomainAsyncJobTypeToString(priv->job.asyncJob),
              obj, obj->def->name);

    qemuDomainObjResetJob(priv);
    if (qemuDomainTrackJob(job))
        qemuDomainObjSaveJob(driver, obj);
    virCondSignal(&priv->job.cond);

    return virObjectUnref(obj);
}

bool
qemuDomainObjEndAsyncJob(virQEMUDriverPtr driver, virDomainObjPtr obj)
{
    qemuDomainObjPrivatePtr priv = obj->privateData;

    priv->jobs_queued--;

    VIR_DEBUG("Stopping async job: %s (vm=%p name=%s)",
              qemuDomainAsyncJobTypeToString(priv->job.asyncJob),
              obj, obj->def->name);

    qemuDomainObjResetAsyncJob(priv);
    qemuDomainObjSaveJob(driver, obj);
    virCondBroadcast(&priv->job.asyncCond);

    return virObjectUnref(obj);
}

void
qemuDomainObjAbortAsyncJob(virDomainObjPtr obj)
{
    qemuDomainObjPrivatePtr priv = obj->privateData;

    VIR_DEBUG("Requesting abort of async job: %s (vm=%p name=%s)",
              qemuDomainAsyncJobTypeToString(priv->job.asyncJob),
              obj, obj->def->name);

    priv->job.asyncAbort = true;
}

static int
libxlDomainObjInitJob(libxlDomainObjPrivatePtr priv)
{
    memset(&priv->job, 0, sizeof(priv->job));

    if (virCondInit(&priv->job.cond) < 0)
        return -1;

    return 0;
}

static void
libxlDomainObjResetJob(libxlDomainObjPrivatePtr priv)
{
    struct libxlDomainJobObj *job = &priv->job;

    job->active = LIBXL_JOB_NONE;
    job->owner = 0;
}

static void
libxlDomainObjFreeJob(libxlDomainObjPrivatePtr priv)
{
    ignore_value(virCondDestroy(&priv->job.cond));
}

/* Give up waiting for mutex after 30 seconds */
#define LIBXL_JOB_WAIT_TIME (1000ull * 30)

/*
 * obj must be locked before calling, libxlDriverPrivatePtr must NOT be locked
 *
 * This must be called by anything that will change the VM state
 * in any way
 *
 * Upon successful return, the object will have its ref count increased,
 * successful calls must be followed by EndJob eventually
 */
int
libxlDomainObjBeginJob(libxlDriverPrivatePtr driver ATTRIBUTE_UNUSED,
                       virDomainObjPtr obj,
                       enum libxlDomainJob job)
{
    libxlDomainObjPrivatePtr priv = obj->privateData;
    unsigned long long now;
    unsigned long long then;

    if (virTimeMillisNow(&now) < 0)
        return -1;
    then = now + LIBXL_JOB_WAIT_TIME;

    virObjectRef(obj);

    while (priv->job.active) {
        VIR_DEBUG("Wait normal job condition for starting job: %s",
                  libxlDomainJobTypeToString(job));
        if (virCondWaitUntil(&priv->job.cond, &obj->parent.lock, then) < 0)
            goto error;
    }

    libxlDomainObjResetJob(priv);

    VIR_DEBUG("Starting job: %s", libxlDomainJobTypeToString(job));
    priv->job.active = job;
    priv->job.owner = virThreadSelfID();

    return 0;

 error:
    VIR_WARN("Cannot start job (%s) for domain %s;"
             " current job is (%s) owned by (%d)",
             libxlDomainJobTypeToString(job),
             obj->def->name,
             libxlDomainJobTypeToString(priv->job.active),
             priv->job.owner);

    if (errno == ETIMEDOUT)
        virReportError(VIR_ERR_OPERATION_TIMEOUT,
                       "%s", _("cannot acquire state change lock"));
    else
        virReportSystemError(errno,
                             "%s", _("cannot acquire job mutex"));

    virObjectUnref(obj);
    return -1;
}

/*
 * obj must be locked before calling
 *
 * To be called after completing the work associated with the
 * earlier libxlDomainBeginJob() call
 *
 * Returns true if the remaining reference count on obj is
 * non-zero, false if the reference count has dropped to zero
 * and obj is disposed.
 */
bool
libxlDomainObjEndJob(libxlDriverPrivatePtr driver ATTRIBUTE_UNUSED,
                     virDomainObjPtr obj)
{
    libxlDomainObjPrivatePtr priv = obj->privateData;
    enum libxlDomainJob job = priv->job.active;

    VIR_DEBUG("Stopping job: %s",
              libxlDomainJobTypeToString(job));

    libxlDomainObjResetJob(priv);
    virCondSignal(&priv->job.cond);

    return virObjectUnref(obj);
}


//example usage
static int
qemuMigrationWaitForCompletion(virQEMUDriverPtr driver, virDomainObjPtr vm,
                               enum qemuDomainAsyncJob asyncJob,
                               virConnectPtr dconn, bool abort_on_error)
{
    qemuDomainObjPrivatePtr priv = vm->privateData;
    const char *job;
    int pauseReason;

    switch (priv->job.asyncJob) {
    case QEMU_ASYNC_JOB_MIGRATION_OUT:
        job = _("migration job");
        break;
    case QEMU_ASYNC_JOB_SAVE:
        job = _("domain save job");
        break;
    case QEMU_ASYNC_JOB_DUMP:
        job = _("domain core dump job");
        break;
    default:
        job = _("job");
    }

    priv->job.info.type = VIR_DOMAIN_JOB_UNBOUNDED;

    while (priv->job.info.type == VIR_DOMAIN_JOB_UNBOUNDED) {
        /* Poll every 50ms for progress & to allow cancellation */
        struct timespec ts = { .tv_sec = 0, .tv_nsec = 50 * 1000 * 1000ull };

        /* cancel migration if disk I/O error is emitted while migrating */
        if (abort_on_error &&
            virDomainObjGetState(vm, &pauseReason) == VIR_DOMAIN_PAUSED &&
            pauseReason == VIR_DOMAIN_PAUSED_IOERROR)
            goto cancel;

        if (qemuMigrationUpdateJobStatus(driver, vm, job, asyncJob) < 0)
            goto cleanup;

        if (dconn && virConnectIsAlive(dconn) <= 0) {
            virReportError(VIR_ERR_OPERATION_FAILED, "%s",
                           _("Lost connection to destination host"));
            goto cleanup;
        }

        virObjectUnlock(vm);

        nanosleep(&ts, NULL);

        virObjectLock(vm);
    }

 cleanup:
    if (priv->job.info.type == VIR_DOMAIN_JOB_COMPLETED)
        return 0;
    else
        return -1;

 cancel:
    if (virDomainObjIsActive(vm)) {
        if (qemuDomainObjEnterMonitorAsync(driver, vm,
                                           priv->job.asyncJob) == 0) {
            qemuMonitorMigrateCancel(priv->mon);
            qemuDomainObjExitMonitor(driver, vm);
        }
    }

    priv->job.info.type = VIR_DOMAIN_JOB_FAILED;
    virReportError(VIR_ERR_OPERATION_FAILED,
                   _("%s: %s"), job, _("failed due to I/O error"));
    return -1;
}
