#include <locale.h>
#include <uchar.h>
#include "../unicode.h"
#include "../text.h"
#include "../templates.h"
using namespace std::literals::string_view_literals;
static constexpr std::string_view hello = R"EOF(
Non-ASCII examples:
  Europe: ¡Hola!, Grüß Gott, Hyvää päivää, Tere õhtust, Bonġu
          Cześć!, Dobrý den, Здравствуйте!, Γειά σας, გამარჯობა
  Africa: ሠላም
  Middle/Near East: שלום, السّلام عليكم
  South Asia: નમસ્તે, नमस्ते, ನಮಸ್ಕಾರ, നമസ്കാരം, ଶୁଣିବେ,
              ආයුබෝවන්, வணக்கம், నమస్కారం, བཀྲ་ཤིས་བདེ་ལེགས༎
  South East Asia: ជំរាបសួរ, ສະບາຍດີ, မင်္ဂလာပါ, สวัสดีครับ, Chào bạn
  East Asia: 你好, 早晨, こんにちは, 안녕하세요
  Misc: Eĥoŝanĝo ĉiuĵaŭde, ⠓⠑⠇⠇⠕, ∀ p ∈ world • hello p  □
  CJK variety: GB(元气,开发), BIG5(元氣,開發), JIS(元気,開発), KSC(元氣,開發)
  Unicode charset: Eĥoŝanĝo ĉiuĵaŭde, Γειά σας, שלום, Здравствуйте!

LANGUAGE (NATIVE NAME)	HELLO
----------------------	-----
Amharic (አማርኛ)	ሠላም
Arabic (العربيّة)	السّلام عليكم
Armenian (հայերեն)	Բարև ձեզ
Bengali (বাংলা)	নমস্কার
Braille	⠓⠑⠇⠇⠕
Burmese (မြန်မာ)	မင်္ဂလာပါ
C	printf ("Hello, world!\n");
Czech (čeština)	Dobrý den
Danish (dansk)	Hej / Goddag / Halløj
Dutch (Nederlands)	Hallo / Dag
Emacs	emacs --no-splash -f view-hello-file
English /ˈɪŋɡlɪʃ/	Hello
Esperanto	Saluton (Eĥoŝanĝo ĉiuĵaŭde)
Estonian (eesti keel)	Tere päevast / Tere õhtust
Finnish (suomi)	Hei / Hyvää päivää
French (français)	Bonjour / Salut
Georgian (ქართველი)	გამარჯობა
German (Deutsch)	Guten Tag / Grüß Gott
Greek (ελληνικά)	Γειά σας
Greek, ancient (ἑλληνική)	Οὖλέ τε καὶ μέγα χαῖρε
Gujarati (ગુજરાતી)	નમસ્તે
Hebrew (עברית)	שלום
Hungarian (magyar)	Szép jó napot!
Hindi (हिंदी)	नमस्ते / नमस्कार ।
Italian (italiano)	Ciao / Buon giorno
Javanese (Jawa)	System.out.println("Sugeng siang!");
Kannada (ಕನ್ನಡ)	ನಮಸ್ಕಾರ
Khmer (ភាសាខ្មែរ)	ជំរាបសួរ
Lao (ພາສາລາວ)	ສະບາຍດີ / ຂໍໃຫ້ໂຊກດີ
Malayalam (മലയാളം)	നമസ്കാരം
Maltese (il-Malti)	Bonġu / Saħħa
Mathematics	∀ p ∈ world • hello p  □
Mongolian (монгол хэл)	Сайн байна уу?
Norwegian (norsk)	Hei / God dag
Oriya (ଓଡ଼ିଆ)	ଶୁଣିବେ
Polish  (język polski)	Dzień dobry! / Cześć!
Russian (русский)	Здра́вствуйте!
Sinhala (සිංහල)	ආයුබෝවන්
Slovak (slovenčina)	Dobrý deň
Slovenian (slovenščina)	Pozdravljeni!
Spanish (español)	¡Hola!
Swedish (svenska)	Hej / Goddag / Hallå
Tamil (தமிழ்)	வணக்கம்
Telugu (తెలుగు)	నమస్కారం
Thai (ภาษาไทย)	สวัสดีครับ / สวัสดีค่ะ
Tibetan (བོད་སྐད་)	བཀྲ་ཤིས་བདེ་ལེགས༎
Tigrigna (ትግርኛ)	ሰላማት
Turkish (Türkçe)	Merhaba
Ukrainian (українська)	Вітаю
Vietnamese (tiếng Việt)	Chào bạn

Japanese (日本語)	こんにちは / ｺﾝﾆﾁﾊ
Chinese (中文,普通话,汉语)	你好
Cantonese (粵語,廣東話)	早晨, 你好
Korean (한글)	안녕하세요 / 안녕하십니까
)EOF"sv;

std::u32string decode_utf8_string_w_iter(const std::string_view str){
  util::utf8_string_iter beg(str.data());
  util::utf8_string_iter end(str.data() + str.size());
  std::u32string ret;
  for(auto c : util::range(beg,end)){
    ret.push_back(c);
  }
  return ret;
}
  

std::string libc_utf8_encode_string(std::u32string_view str){
  //Only works if in utf8 locale.
  std::string ret; 
  mbstate_t state{};
  char buf[MB_CUR_MAX];
  for(size_t n = 0; n < str.size(); ++n){
    int rc = c32rtomb(buf, str[n], &state);
    if(rc == -1){ return u8""; }
    ret.append(buf);
  }
  return ret;
}
std::u32string libc_utf8_decode_string(std::string_view str){
  std::mbstate_t state{}; // zero-initialized to initial state
  std::u32string ret;
  char32_t c32;
  const char *ptr = str.data(), *end = str.data() + str.size();
  int rc;
  while(ptr < end){
    int rc = mbrtoc32(&c32, ptr, (end+1) - ptr, &state);
    if(rc < 0){ return U""; }
    ret.push_back(c32);
    ptr += rc;
  }
  return ret;
}
bool do_cmp(const std::u32string_view &control,
            const std::u32string_view &test){
  if(control.size() != test.size()){
    fprintf(stderr, "Strings have different sizes: control %lu, test %lu.\n",
            control.size(), test.size());
    return false;
  }
  for(ssize_t i = 0; i < control.size(); i++){
    if(control[i] != test[i]){
      int start = std::max(i-10, (ssize_t)0);
      int end = std::min(i + 10, (ssize_t)control.size());
      auto control_sub = control.substr(start, end-start);
      auto test_sub = test.substr(start, end-start);
      fprintf(stderr, "Mismatch at index %d:\n"
              "control: %.*ls.\n"
              "test   : %.*ls.\n", i,
              (int)control_sub.size(),control_sub.data(),
              (int)test_sub.size(), test_sub.data());
      return false;
    }
  }
  fprintf(stderr, "Strings match.\n");
  return true;
}
int main(){
  //ensure c32rtomb gives us unicode, this is obviously nonportable.
  setlocale(LC_ALL, "en_US.UTF-8");
  fprintf(stderr,"Decoding using libc.\n");
  auto libc_str = libc_utf8_decode_string(hello);
  fprintf(stderr,"Decoding using util::utf8_decode_string.\n");
  auto my_str = util::utf8_decode_string(hello);
  fprintf(stderr,"Decoding using util::utf8_string_iter.\n");
  auto my_str2 = decode_utf8_string_w_iter(hello);
  fprintf(stderr,"Comparing libc and function decoding.\n");
  do_cmp(libc_str, my_str);
  fprintf(stderr,"Comparing libc and iterator decoding.\n");
  do_cmp(libc_str, my_str2);
  FILE* libc_file = fopen("libc_str.out", "w");
  FILE* my_str_file = fopen("my_str.out", "w");
  FILE* my_str2_file = fopen("my_str2.out", "w");
  fwrite(libc_str.data(), libc_str.size(), sizeof(libc_str[0]), libc_file);
  fwrite(my_str.data(), my_str.size(), sizeof(my_str[0]), my_str_file);
  fwrite(my_str2.data(), my_str2.size(), sizeof(my_str2[0]), my_str2_file);
  fclose(libc_file);
  fclose(my_str_file);
  fclose(my_str2_file);
  return 0;
}
