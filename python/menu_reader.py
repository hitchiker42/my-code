import argparse, shutil, sys, os
def decode(s):
  return s.decode('utf-8')
def utf8_bytes(s):
  bytes(s, encoding='utf-8')

def debug_print(*args):
  print(*args)

def read_7_bit_int(f):
  count = 0
  shift = 0
  #No do while in python, of course
  while True:
    if shift > 32:
      raise ValueError("Malformed 7 bit encoded integer at offset {}".format(f.tell() - 5))
    b = f.read(1)[0]
    count |= (b & 0x7f) << shift
    shift += 7
    if not (b & 0x80):
      break
  return count
def read_int(f):
  b = f.read(4)
  return int.from_bytes(b, byteorder="little", signed = "True")

def encode_7_bit_int(i):
  b = bytearray()
  while(i > 0x80):
    b.append((i & 0x7f) | 0x80)
    i >>= 7
  b.append(i)
  return bytes(b)
def read_csharp_string(f):
  size = read_7_bit_int(f)
  return f.read(size)

def write_csharp_string(f, s):
  f.write(encode_7_bit_int(len(s)))
  f.write(s)

def read_header(f):
  sz = f.read(1)[0]
  fid = f.read(sz)
  if fid != b'CM3D2_MENU':
    raise TypeError("Not a menu file.")
  version = read_int(f)
  path = read_csharp_string(f)
  name = read_csharp_string(f)
  category = read_csharp_string(f)
  setumei = read_csharp_string(f)
  return [version, path, name, category, setumei]

def write_header(f, metadata):
  f.write(b'\x0aCM3D2_MENU')
  f.write(metadata[0].to_bytes(4, 'little'))
  for s in metadata[1:]:
    write_csharp_string(f, s)

def read_data_array(f):
  sz = f.read(1)[0]
  return [read_csharp_string(f) for _ in range(sz)]

def read_menu_file(f):
  metadata = read_header(f)
  data_size = read_int(f)
  data_end = (f.tell() - 1) + data_size
  data = []
  while True:
    data.append(read_data_array(f))
    if(f.tell() == data_end):
        break
    elif(f.tell() > data_end):
      f.seek(0, os.SEEK_END)
      sz = f.tell()-data_end
      # No RangeError, maybe should be value error
      raise IndexError("Data section {} bytes longer than expected".format(sz))
  return (data, data_size, metadata)

def write_menu_file(path, metadata, data, data_size):
  with open(path, 'wb') as f:
    write_header(f, metadata)
    f.write(data_size.to_bytes(4, 'little'))
    for arr in data:
      f.write(len(arr).to_bytes(1,'little'))
      for s in arr:
        write_csharp_string(f, s)

# Both append_to and remove_from menu_file implicitly assume that no string
# being added/removed is longer than 127 bytes, it should be eaisy enough to fix
# this if it becomes an issue.
def append_to_menu_file(path, new_data):
  with open(path, 'rb') as f:
    (data, data_size, metadata) = read_menu_file(f)
    # in theory we could just update data_size inplace and
    # append to the file, but eh, this is eaiser / less error prone
  data.append(new_data)
  data_size += sum(map(len, new_data)) + len(new_data) + 1
  print("Data size:{}\ndata:{}".format(data_size, data))
  write_menu_file(path, metadata, data, data_size)

def remove_from_menu_file(path, to_remove):
  with open(path, 'rb') as f:
    (data, data_size, metadata) = read_menu_file(f)
    # in theory we could just update data_size inplace and
    # append to the file, but eh, this is eaiser / less error prone
  i = 0
  while i < len(data):
    if(data[i] in to_remove):
      data_size -= sum(map(len, data[i])) + len(data[i]) + 1
      # Avoid breaking if we're on the last element
      tmp = data.pop()
      if i < len(data):
        data[i] = tmp
    else:
      i += 1
  print("Data size:{}\ndata:{}".format(data_size, data))
  write_menu_file(path, metadata, data, data_size)

def display_menu_file(metadata, data, size):
  print("version:{}\npath:{}\nname:{}\ncategory:{}\nsetumei:{}"
        .format(metadata[0],
                *map(decode, metadata[1:])))
  print("Data size: {} bytes".format(size))
  for arr in data:
    print(decode(arr[0]))
    for s in arr[1:]:
      print("  -", decode(s))
def print_menu_file(path):
  with open(path, 'rb') as f:
    (data, data_size, metadata) = read_menu_file(f)
  display_menu_file(metadata, data, data_size)

if __name__ == "__main__":
  parser = argparse.ArgumentParser(
    description="Add or remove a given value to a set of cm3d2 menu files")
  group = parser.add_mutually_exclusive_group()
  parser.add_argument('paths', nargs='+',
                      help="List of menu files to modify")
  group.add_argument('-v','--value',
                      help="Value to add, seperate multiple items using ':' (May change escape if needed)")
  group.add_argument('-r','--remove', action='append',
                      help="Value to remove, seperate multiple items using ':', may be passed multiple times")
  parser.add_argument('-d', '--display', action='store_true',
                      help="Print the menu files, after appending value(s) if applicable")
  parser.add_argument('-b', '--backup', action='store_true',
                      help="Backup original files before modifying them")
  args = parser.parse_args()
  for path in args.paths:
    if args.backup and (args.value or args.remove):
      shutil.copy(path, path.join('.bkup'))
    if args.value:
      values = args.value.encode('utf-8').split(b':')
      print(values)
      append_to_menu_file(path, values)
    if args.remove:
      values = [x.encode('utf-8').split(b':') for x in args.remove]
      print(values)
      remove_from_menu_file(path, values)
    if args.display:
      print_menu_file(path)
