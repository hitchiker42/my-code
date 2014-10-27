; ModuleID = 'alarmd_client.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }
%struct.my_alarm = type { i64, i8*, i32, i8, i8, i8, i8, i32 }
%union.pthread_attr_t = type { i64, [48 x i8] }
%struct.sockaddr_un = type { i16, [108 x i8] }
%struct.sockaddr = type { i16, [14 x i8] }

@repeat_opt = global [10 x i8] c"-loop 001\00", align 1
@sock = internal global i32 0, align 4
@.str = private unnamed_addr constant [29 x i8] c"/var/run/alarmdalarmd_socket\00", align 1
@.str1 = private unnamed_addr constant [4 x i8] c"add\00", align 1
@stderr = external global %struct._IO_FILE*
@.str2 = private unnamed_addr constant [36 x i8] c"Invalid command %s did you mean %s\0A\00", align 1
@.str3 = private unnamed_addr constant [6 x i8] c"clear\00", align 1
@.str4 = private unnamed_addr constant [7 x i8] c"delete\00", align 1
@.str5 = private unnamed_addr constant [5 x i8] c"list\00", align 1
@.str6 = private unnamed_addr constant [7 x i8] c"modify\00", align 1
@.str7 = private unnamed_addr constant [7 x i8] c"snooze\00", align 1
@.str8 = private unnamed_addr constant [5 x i8] c"stop\00", align 1
@.str9 = private unnamed_addr constant [20 x i8] c"Invalid command %s\0A\00", align 1
@alarm_socket = common global i32 0, align 4
@alarm_queue = common global %struct.my_alarm** null, align 8
@command_process = common global i32 0, align 4
@detached_attr = common global %union.pthread_attr_t zeroinitializer, align 8
@main_thread = common global i64 0, align 8
@next_alarm = common global i64 0, align 8
@queue_length = common global i32 0, align 4
@queue_size = common global i32 0, align 4
@.str10 = private unnamed_addr constant [7 x i8] c"socket\00", align 1
@.str11 = private unnamed_addr constant [5 x i8] c"bind\00", align 1

; Function Attrs: noreturn nounwind uwtable
define void @help() #0 {
  call void @exit(i32 0) #7
  unreachable
                                                  ; No predecessors!
  ret void
}

; Function Attrs: noreturn nounwind
declare void @exit(i32) #1

; Function Attrs: noreturn nounwind uwtable
define void @add_alarm(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i8**, align 8
  store i32 %argc, i32* %1, align 4
  store i8** %argv, i8*** %2, align 8
  call void @exit(i32 0) #7
  unreachable
                                                  ; No predecessors!
  ret void
}

; Function Attrs: noreturn nounwind uwtable
define void @list_alarm(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i8**, align 8
  store i32 %argc, i32* %1, align 4
  store i8** %argv, i8*** %2, align 8
  call void @exit(i32 0) #7
  unreachable
                                                  ; No predecessors!
  ret void
}

; Function Attrs: noreturn nounwind uwtable
define void @delete_alarm(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i8**, align 8
  store i32 %argc, i32* %1, align 4
  store i8** %argv, i8*** %2, align 8
  call void @exit(i32 0) #7
  unreachable
                                                  ; No predecessors!
  ret void
}

; Function Attrs: noreturn nounwind uwtable
define void @stop_alarm(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i8**, align 8
  %action = alloca i32, align 4
  store i32 %argc, i32* %1, align 4
  store i8** %argv, i8*** %2, align 8
  store i32 8, i32* %action, align 4
  %3 = load i32* @sock, align 4
  %4 = bitcast i32* %action to i8*
  %5 = call i64 @write(i32 %3, i8* %4, i64 4)
  call void @exit(i32 0) #7
  unreachable
                                                  ; No predecessors!
  ret void
}

declare i64 @write(i32, i8*, i64) #2

; Function Attrs: noreturn nounwind uwtable
define void @clear_alarm(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i8**, align 8
  %action = alloca i32, align 4
  store i32 %argc, i32* %1, align 4
  store i8** %argv, i8*** %2, align 8
  store i32 1, i32* %action, align 4
  %3 = load i32* @sock, align 4
  %4 = bitcast i32* %action to i8*
  %5 = call i64 @write(i32 %3, i8* %4, i64 4)
  call void @exit(i32 0) #7
  unreachable
                                                  ; No predecessors!
  ret void
}

; Function Attrs: nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #3 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 8
  store i32 0, i32* %1
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 8
  %4 = load i8*** %3, align 8
  %5 = getelementptr inbounds i8** %4, i64 1
  %6 = load i8** %5, align 8
  %7 = getelementptr inbounds i8* %6, i64 0
  %8 = load i8* %7, align 1
  %9 = sext i8 %8 to i32
  %10 = icmp eq i32 %9, 45
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %0
  br label %12

; <label>:12                                      ; preds = %11, %0
  %13 = call i32 @make_alarm_socket(i8* getelementptr inbounds ([29 x i8]* @.str, i32 0, i32 0), i32 1)
  store i32 %13, i32* @sock, align 4
  %14 = load i8*** %3, align 8
  %15 = getelementptr inbounds i8** %14, i64 1
  %16 = load i8** %15, align 8
  %17 = getelementptr inbounds i8* %16, i64 0
  %18 = load i8* %17, align 1
  %19 = sext i8 %18 to i32
  switch i32 %19, label %135 [
    i32 97, label %20
    i32 101, label %37
    i32 100, label %54
    i32 108, label %71
    i32 109, label %88
    i32 115, label %105
  ]

; <label>:20                                      ; preds = %12
  %21 = load i8*** %3, align 8
  %22 = getelementptr inbounds i8** %21, i64 1
  %23 = load i8** %22, align 8
  %24 = call i32 @strcmp(i8* %23, i8* getelementptr inbounds ([4 x i8]* @.str1, i32 0, i32 0)) #8
  %25 = icmp ne i32 %24, 0
  br i1 %25, label %31, label %26

; <label>:26                                      ; preds = %20
  %27 = load i32* %2, align 4
  %28 = sub nsw i32 %27, 1
  %29 = load i8*** %3, align 8
  %30 = getelementptr inbounds i8** %29, i64 1
  call void @add_alarm(i32 %28, i8** %30) #9
  unreachable

; <label>:31                                      ; preds = %20
  %32 = load %struct._IO_FILE** @stderr, align 8
  %33 = load i8*** %3, align 8
  %34 = getelementptr inbounds i8** %33, i64 1
  %35 = load i8** %34, align 8
  %36 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %32, i8* getelementptr inbounds ([36 x i8]* @.str2, i32 0, i32 0), i8* %35, i8* getelementptr inbounds ([4 x i8]* @.str1, i32 0, i32 0))
  call void @exit(i32 1) #7
  unreachable

; <label>:37                                      ; preds = %12
  %38 = load i8*** %3, align 8
  %39 = getelementptr inbounds i8** %38, i64 1
  %40 = load i8** %39, align 8
  %41 = call i32 @strcmp(i8* %40, i8* getelementptr inbounds ([6 x i8]* @.str3, i32 0, i32 0)) #8
  %42 = icmp ne i32 %41, 0
  br i1 %42, label %48, label %43

; <label>:43                                      ; preds = %37
  %44 = load i32* %2, align 4
  %45 = sub nsw i32 %44, 1
  %46 = load i8*** %3, align 8
  %47 = getelementptr inbounds i8** %46, i64 1
  call void @clear_alarm(i32 %45, i8** %47) #9
  unreachable

; <label>:48                                      ; preds = %37
  %49 = load %struct._IO_FILE** @stderr, align 8
  %50 = load i8*** %3, align 8
  %51 = getelementptr inbounds i8** %50, i64 1
  %52 = load i8** %51, align 8
  %53 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %49, i8* getelementptr inbounds ([36 x i8]* @.str2, i32 0, i32 0), i8* %52, i8* getelementptr inbounds ([6 x i8]* @.str3, i32 0, i32 0))
  call void @exit(i32 1) #7
  unreachable

; <label>:54                                      ; preds = %12
  %55 = load i8*** %3, align 8
  %56 = getelementptr inbounds i8** %55, i64 1
  %57 = load i8** %56, align 8
  %58 = call i32 @strcmp(i8* %57, i8* getelementptr inbounds ([7 x i8]* @.str4, i32 0, i32 0)) #8
  %59 = icmp ne i32 %58, 0
  br i1 %59, label %65, label %60

; <label>:60                                      ; preds = %54
  %61 = load i32* %2, align 4
  %62 = sub nsw i32 %61, 1
  %63 = load i8*** %3, align 8
  %64 = getelementptr inbounds i8** %63, i64 1
  call void @delete_alarm(i32 %62, i8** %64) #9
  unreachable

; <label>:65                                      ; preds = %54
  %66 = load %struct._IO_FILE** @stderr, align 8
  %67 = load i8*** %3, align 8
  %68 = getelementptr inbounds i8** %67, i64 1
  %69 = load i8** %68, align 8
  %70 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %66, i8* getelementptr inbounds ([36 x i8]* @.str2, i32 0, i32 0), i8* %69, i8* getelementptr inbounds ([7 x i8]* @.str4, i32 0, i32 0))
  call void @exit(i32 1) #7
  unreachable

; <label>:71                                      ; preds = %12
  %72 = load i8*** %3, align 8
  %73 = getelementptr inbounds i8** %72, i64 1
  %74 = load i8** %73, align 8
  %75 = call i32 @strcmp(i8* %74, i8* getelementptr inbounds ([5 x i8]* @.str5, i32 0, i32 0)) #8
  %76 = icmp ne i32 %75, 0
  br i1 %76, label %82, label %77

; <label>:77                                      ; preds = %71
  %78 = load i32* %2, align 4
  %79 = sub nsw i32 %78, 1
  %80 = load i8*** %3, align 8
  %81 = getelementptr inbounds i8** %80, i64 1
  call void @list_alarm(i32 %79, i8** %81) #9
  unreachable

; <label>:82                                      ; preds = %71
  %83 = load %struct._IO_FILE** @stderr, align 8
  %84 = load i8*** %3, align 8
  %85 = getelementptr inbounds i8** %84, i64 1
  %86 = load i8** %85, align 8
  %87 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %83, i8* getelementptr inbounds ([36 x i8]* @.str2, i32 0, i32 0), i8* %86, i8* getelementptr inbounds ([5 x i8]* @.str5, i32 0, i32 0))
  call void @exit(i32 1) #7
  unreachable

; <label>:88                                      ; preds = %12
  %89 = load i8*** %3, align 8
  %90 = getelementptr inbounds i8** %89, i64 1
  %91 = load i8** %90, align 8
  %92 = call i32 @strcmp(i8* %91, i8* getelementptr inbounds ([7 x i8]* @.str6, i32 0, i32 0)) #8
  %93 = icmp ne i32 %92, 0
  br i1 %93, label %99, label %94

; <label>:94                                      ; preds = %88
  %95 = load i32* %2, align 4
  %96 = sub nsw i32 %95, 1
  %97 = load i8*** %3, align 8
  %98 = getelementptr inbounds i8** %97, i64 1
  call void @modify_alarm(i32 %96, i8** %98) #9
  unreachable

; <label>:99                                      ; preds = %88
  %100 = load %struct._IO_FILE** @stderr, align 8
  %101 = load i8*** %3, align 8
  %102 = getelementptr inbounds i8** %101, i64 1
  %103 = load i8** %102, align 8
  %104 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %100, i8* getelementptr inbounds ([36 x i8]* @.str2, i32 0, i32 0), i8* %103, i8* getelementptr inbounds ([7 x i8]* @.str6, i32 0, i32 0))
  call void @exit(i32 1) #7
  unreachable

; <label>:105                                     ; preds = %12
  %106 = load i8*** %3, align 8
  %107 = getelementptr inbounds i8** %106, i64 1
  %108 = load i8** %107, align 8
  %109 = call i32 @strcmp(i8* %108, i8* getelementptr inbounds ([7 x i8]* @.str7, i32 0, i32 0)) #8
  %110 = icmp ne i32 %109, 0
  br i1 %110, label %116, label %111

; <label>:111                                     ; preds = %105
  %112 = load i32* %2, align 4
  %113 = sub nsw i32 %112, 1
  %114 = load i8*** %3, align 8
  %115 = getelementptr inbounds i8** %114, i64 1
  call void @snooze_alarm(i32 %113, i8** %115) #9
  unreachable

; <label>:116                                     ; preds = %105
  %117 = load i8*** %3, align 8
  %118 = getelementptr inbounds i8** %117, i64 1
  %119 = load i8** %118, align 8
  %120 = call i32 @strcmp(i8* %119, i8* getelementptr inbounds ([5 x i8]* @.str8, i32 0, i32 0)) #8
  %121 = icmp ne i32 %120, 0
  br i1 %121, label %127, label %122

; <label>:122                                     ; preds = %116
  %123 = load i32* %2, align 4
  %124 = sub nsw i32 %123, 1
  %125 = load i8*** %3, align 8
  %126 = getelementptr inbounds i8** %125, i64 1
  call void @stop_alarm(i32 %124, i8** %126) #9
  unreachable

; <label>:127                                     ; preds = %116
  %128 = load %struct._IO_FILE** @stderr, align 8
  %129 = load i8*** %3, align 8
  %130 = getelementptr inbounds i8** %129, i64 1
  %131 = load i8** %130, align 8
  %132 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %128, i8* getelementptr inbounds ([20 x i8]* @.str9, i32 0, i32 0), i8* %131)
  br label %133

; <label>:133                                     ; preds = %127
  br label %134

; <label>:134                                     ; preds = %133
  br label %135

; <label>:135                                     ; preds = %12, %134
  %136 = load %struct._IO_FILE** @stderr, align 8
  %137 = load i8*** %3, align 8
  %138 = getelementptr inbounds i8** %137, i64 1
  %139 = load i8** %138, align 8
  %140 = call i32 (%struct._IO_FILE*, i8*, ...)* @fprintf(%struct._IO_FILE* %136, i8* getelementptr inbounds ([20 x i8]* @.str9, i32 0, i32 0), i8* %139)
  call void @exit(i32 1) #7
  unreachable
                                                  ; No predecessors!
  %142 = load i32* %1
  ret i32 %142
}

; Function Attrs: nounwind uwtable
define internal i32 @make_alarm_socket(i8* %filename, i32 %mode) #3 {
  %1 = alloca i32, align 4
  %2 = alloca i8*, align 8
  %3 = alloca i32, align 4
  %sock = alloca i32, align 4
  %size = alloca i64, align 8
  %name = alloca %struct.sockaddr_un, align 2
  store i8* %filename, i8** %2, align 8
  store i32 %mode, i32* %3, align 4
  %4 = call i32 @socket(i32 1, i32 1, i32 0) #10
  store i32 %4, i32* %sock, align 4
  %5 = load i32* %sock, align 4
  %6 = icmp slt i32 %5, 0
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  call void @perror(i8* getelementptr inbounds ([7 x i8]* @.str10, i32 0, i32 0))
  call void @exit(i32 1) #7
  unreachable

; <label>:8                                       ; preds = %0
  %9 = getelementptr inbounds %struct.sockaddr_un* %name, i32 0, i32 0
  store i16 1, i16* %9, align 2
  %10 = getelementptr inbounds %struct.sockaddr_un* %name, i32 0, i32 1
  %11 = getelementptr inbounds [108 x i8]* %10, i32 0, i32 0
  %12 = load i8** %2, align 8
  %13 = call i8* @strncpy(i8* %11, i8* %12, i64 108) #10
  %14 = getelementptr inbounds %struct.sockaddr_un* %name, i32 0, i32 1
  %15 = getelementptr inbounds [108 x i8]* %14, i32 0, i64 107
  store i8 0, i8* %15, align 1
  %16 = getelementptr inbounds %struct.sockaddr_un* %name, i32 0, i32 1
  %17 = getelementptr inbounds [108 x i8]* %16, i32 0, i32 0
  %18 = call i64 @strlen(i8* %17) #8
  %19 = add i64 2, %18
  store i64 %19, i64* %size, align 8
  %20 = load i32* %3, align 4
  switch i32 %20, label %41 [
    i32 0, label %21
    i32 1, label %31
  ]

; <label>:21                                      ; preds = %8
  %22 = load i32* %sock, align 4
  %23 = bitcast %struct.sockaddr_un* %name to %struct.sockaddr*
  %24 = load i64* %size, align 8
  %25 = trunc i64 %24 to i32
  %26 = call i32 @bind(i32 %22, %struct.sockaddr* %23, i32 %25) #10
  %27 = icmp slt i32 %26, 0
  br i1 %27, label %28, label %29

; <label>:28                                      ; preds = %21
  call void @perror(i8* getelementptr inbounds ([5 x i8]* @.str11, i32 0, i32 0))
  call void @exit(i32 1) #7
  unreachable

; <label>:29                                      ; preds = %21
  %30 = load i32* %sock, align 4
  store i32 %30, i32* %1
  br label %41

; <label>:31                                      ; preds = %8
  %32 = load i32* %sock, align 4
  %33 = bitcast %struct.sockaddr_un* %name to %struct.sockaddr*
  %34 = load i64* %size, align 8
  %35 = trunc i64 %34 to i32
  %36 = call i32 @connect(i32 %32, %struct.sockaddr* %33, i32 %35)
  %37 = icmp slt i32 %36, 0
  br i1 %37, label %38, label %39

; <label>:38                                      ; preds = %31
  call void @perror(i8* getelementptr inbounds ([5 x i8]* @.str11, i32 0, i32 0))
  call void @exit(i32 1) #7
  unreachable

; <label>:39                                      ; preds = %31
  %40 = load i32* %sock, align 4
  store i32 %40, i32* %1
  br label %41

; <label>:41                                      ; preds = %29, %39, %8
  %42 = load i32* %1
  ret i32 %42
}

; Function Attrs: nounwind readonly
declare i32 @strcmp(i8*, i8*) #4

declare i32 @fprintf(%struct._IO_FILE*, i8*, ...) #2

; Function Attrs: noreturn
declare void @modify_alarm(i32, i8**) #5

; Function Attrs: noreturn
declare void @snooze_alarm(i32, i8**) #5

; Function Attrs: nounwind
declare i32 @socket(i32, i32, i32) #6

declare void @perror(i8*) #2

; Function Attrs: nounwind
declare i8* @strncpy(i8*, i8*, i64) #6

; Function Attrs: nounwind readonly
declare i64 @strlen(i8*) #4

; Function Attrs: nounwind
declare i32 @bind(i32, %struct.sockaddr*, i32) #6

declare i32 @connect(i32, %struct.sockaddr*, i32) #2

attributes #0 = { noreturn nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { noreturn nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind readonly "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { noreturn "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #7 = { noreturn nounwind }
attributes #8 = { nounwind readonly }
attributes #9 = { noreturn }
attributes #10 = { nounwind }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4 (tags/RELEASE_34/final)"}
