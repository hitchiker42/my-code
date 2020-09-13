import fileinput
import sys, os, os.path

def main():
  if len(sys.argv) <= 1:
    print("Requires at least 1 file as input")
    return
  dupefile = sys.argv[1]
  basedir = os.getcwd()
  if len(sys.argv) >= 3:
    basedir = sys.argv[2]
  dupes = {}
  #TODO: Check if file is utf16 or shift-jis
  for line in fileinput.input(dupefile,
                              openhook=fileinput.hook_encoded("utf-16")):
    line = line.rstrip('\r\n')
    dupes[line] = []
  os.chdir(basedir)
  for (rootdir, subdirs, files) in os.walk(basedir):
    for file in files:
      if file in dupes:
        dupes[file].append(os.path.join(rootdir, file))
  out = open("com3d2_dupes_fullpath.log", "w", encoding="utf-16")
  for dupe in dupes:
    paths = dupes[dupe]
    print(dupe, file=out)
    for path in paths:
      print(path, file=out)
    print(file=out)
  return

if __name__ ==  "__main__":
  main()
