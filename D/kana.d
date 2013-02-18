/*Tucker DiNapoli, Kana quiz program, last modified 10/21/12
 *A simple interactive kana quiz, writes a random kana character to stdout and reads in a guess
 *if the guess is correct it repeats,if incorrect it allows upto 5 more guesses
 *to exit simply enter exit as your guess
 */
import std.random,std.c.stdio,std.stdio;
int main(){
const string[46] meaning=
  ["a","i","u","e","o",
  "ka","ki","ku","ke","ko",
  "sa","shi","su","se","so",
  "ta","chi","tsu","te","to",
  "na","ni","nu","ne","no",
  "ha","hi","fu","he","ho",
  "ma","mi","mu","me","mo",
  "ya","yu","yo",
  "ra","ri","ru","re","ro",
  "wa","wo","n"];

const string[46] hiragana=
        ["あ","い","う","え","お",
"か","き","く","け","こ",
"さ","し","す","せ","そ",
"た","ち","つ","て","と",
"な","に","ぬ","ね","の",
"は","ひ","ふ","へ","ほ",
"ま","み","む","め","も",
"や","ゆ","よ",
"ら","り","る","れ","ろ",
"わ","を","ん"];
const string[46] katakana=
        ["ア","イ","ウ","エ","オ",
"カ","キ","ク","ケ","コ",
"サ","シ","ス","セ","ソ",
"タ","チ","ツ","テ","ト",
"ナ","ニ","ヌ","ネ","ノ",
"ハ","ヒ","フ","ヘ","ホ",
"マ","ミ","ム","メ","モ",
"ヤ","ユ","ヨ",
"ラ","リ","ル","レ","ロ",
"ワ","ヲ","ン"];

  int quiz,cnt;
  string  guess;
  string[46] kana;
  while(true){
    quiz=uniform(0,45);
    cnt=1;
    if (quiz%2==1)
      kana=katakana;
    else
      kana=hiragana;
    auto ans=kana[quiz];
    writefln("\nWhat is the meaing of %s",ans);
    readf("%s",guess);
    writefln("%s",guess);
    writefln("%s",meaning[quiz]);
    while (cnt>=0){
      if (guess=="exit"||guess=="Exit"){
          return(0);
    }
      if (guess==meaning[quiz]){
  writefln("\nCorrect!");
  cnt=-1;
      }
      else if (cnt<5){
  writefln("\nWrong guess again");
  readf("%s",guess);
  cnt++;
      }
      else{
  writefln("\nSorry, too many guesses, try again");
  cnt=-1;
      }
    }
  }
}
