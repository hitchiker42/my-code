#!/usr/bin/python3

#Tucker DiNapoli, kana quiz, Date Created 10/22/12,
#When run prompts the user with a random japanese kana
#and accepts a string to be compaired with it's latin representation
#if correct it repeats, if incorrect it gives up to 5 more chances
#after 5 tries or if the guess is left blank it will print the correct answer
#and move on to a new kana
#if at any time the string exit or Exit is entered the program exits

from random import choice
from sys import argv
def kana_quiz(argv):
    kana={'あ':'a','い':'i','う':'u','え':'e','お':'o',
          'か':'ka','き':'ki','く':'ku','け':'ke','こ':'ko',
          'さ':'sa','し':'shi','す':'su','せ':'se','そ':'so',
          'た':'ta','ち':'chi','つ':'tsu','て':'te','と':'to',
          'な':'na','に':'ni','ぬ':'nu','ね':'ne','の':'no',
          'は':'ha','ひ':'hi','ふ':'fu','へ':'he','ほ':'ho',
          'ま':'ma','み':'mi','む':'mu','め':'me','も':'mo',
          'や':'ya','ゆ':'yu','よ':'yo',
          'ら':'ra','り':'ri','る':'ru','れ':'re','ろ':'ro',
          'わ':'wa','を':'wo','ん':'n',
          'が':'ga','ぎ':'gi','ぐ':'gu','げ':'ge','ご':'go',
          'ざ':'za','じ':'ji','ず':'zu','ぜ':'ze','ぞ':'zo',
          'だ':'da','で':'de','ど':'do',
          'ば':'ba','び':'bi','ぶ':'bu','べ':'be','ぼ':'bo',
          'ぱ':'pa','ぴ':'pi','ぷ':'pu','ぺ':'pe','ぽ':'po',
#hiragara above kataka below
          'ア':'a','イ':'i','ウ':'u','エ':'e','オ':'o',
          'カ':'ka','キ':'ki','ク':'ku','ケ':'ke','コ':'ko',
          'サ':'sa','シ':'shi','ス':'su','セ':'se','ソ':'so',
          'タ':'ta','チ':'chi','ツ':'tsu','テ':'te','ト':'to',
          'ナ':'na','ニ':'ni','ヌ':'nu','ネ':'ne','ノ':'no',
          'ハ':'ha','ヒ':'hi','フ':'fu','ヘ':'he','ホ':'ho',
          'マ':'ma','ミ':'mi','ム':'mu','メ':'me','モ':'mo',
          'ヤ':'ya','ユ':'yu','ヨ':'yo',
          'ラ':'ra','リ':'ri','ル':'ru','レ':'re','ロ':'ro',
          'ワ':'wa','ヲ':'wo','ン':'n',
          'ガ':'ga','ギ':'gi','グ':'gu','ゲ':'ge','ゴ':'go',
          'ザ':'za','ジ':'ji','ズ':'zu','ゼ':'ze','ゾ':'zo',
          'ダ':'da','ヂ':'ji','ヅ':'zu','デ':'de','ド':'do',
          'バ':'ba','ビ':'bi','ブ':'bu','ベ':'be','ボ':'bo',
          'パ':'pa','ピ':'pi','プ':'pu','ペ':'pe','ポ':'po'}
    hiragana={'あ':'a','い':'i','う':'u','え':'e','お':'o',
              'か':'ka','き':'ki','く':'ku','け':'ke','こ':'ko',
              'さ':'sa','し':'shi','す':'su','せ':'se','そ':'so',
              'た':'ta','ち':'chi','つ':'tsu','て':'te','と':'to',
              'な':'na','に':'ni','ぬ':'nu','ね':'ne','の':'no',
              'は':'ha','ひ':'hi','ふ':'fu','へ':'he','ほ':'ho',
              'ま':'ma','み':'mi','む':'mu','め':'me','も':'mo',
              'や':'ya','ゆ':'yu','よ':'yo',
              'ら':'ra','り':'ri','る':'ru','れ':'re','ろ':'ro',
              'わ':'wa','を':'wo','ん':'n'}
    katakana={'ア':'a','イ':'i','ウ':'u','エ':'e','オ':'o',
              'カ':'ka','キ':'ki','ク':'ku','ケ':'ke','コ':'ko',
              'サ':'sa','シ':'shi','ス':'su','セ':'se','ソ':'so',
              'タ':'ta','チ':'chi','ツ':'tsu','テ':'te','ト':'to',
              'ナ':'na','ニ':'ni','ヌ':'nu','ネ':'ne','ノ':'no',
              'ハ':'ha','ヒ':'hi','フ':'fu','ヘ':'he','ホ':'ho',
              'マ':'ma','ミ':'mi','ム':'mu','メ':'me','モ':'mo',
              'ヤ':'ya','ユ':'yu','ヨ':'yo',
              'ラ':'ra','リ':'ri','ル':'ru','レ':'re','ロ':'ro',
              'ワ':'wa','ヲ':'wo','ン':'n'}
    kana_eng={'a':['あ','ア'],'i':['い','イ'],'o':['お','オ'],'u':['う','ウ'],'e':['え','エ'],
    'fu':['ふ','フ'],'ha':['は','ハ'],'he':['へ','ヘ'],'hi':['ひ','ヒ'],'ho':['ほ','ホ'],
    'ka':['か','カ'],'ke':['け','ケ'],'ki':['き','キ'],'ko':['こ','コ'],'ku':['く','ク'],
    'ma':['ま','マ'],'me':['め','メ'],'mi':['み','ミ'],'mo':['も','モ'],'mu':['む','ム'],
    'na':['な','ナ'],'ne':['ね','ネ'],'ni':['に','ニ'],'no':['の','ノ'],'nu':['ぬ','ヌ'],
    'ra':['ら','ラ'],'re':['れ','レ'],'ri':['り','リ'],'ro':['ろ','ロ'],'ru':['る','ル'],
    'sa':['さ','サ'],'se':['せ','セ'],'shi':['し','シ'],'so':['そ','ソ'],'su':['す','ス'],
    'ta':['た','タ'],'chi':['ち','チ'],'te':['て','テ'],'to':['と','ト'],'tsu':['つ','ツ'],
    'wa':['わ','ワ'],'wo':['を','ヲ'],'n':['ん','ン'],
    'ya':['や','ヤ'],'yo':['よ','ヨ'],'yu':['ゆ','ユ']}
    guess=''
    score=1
    eng=False
    try:
        q=argv[1]
        if q=='table':
            table()
            q=argv[2]
        if q=='katakana' or q=='k':
            kana=katakana
        elif q=='hiragana' or q=='h':
            kana=hiragana
        elif q=='eng' or q=='e':
            kana=kana_eng
            eng=True
    except:
        IndexError
    while (True):
        cnt=0
        quiz=choice(list(iter(kana)))
        print("\nIdentify this kana:",quiz)
        guess=input()
        while(not cnt<0):
            if (guess=='exit' or guess=='Exit'):
                print("\nGoodbye!\n")
                return()
            elif (guess=='h'):
                table('h')
                guess=input()
            elif (guess=='k'):
                table('k')
                guess=input()
            elif(guess=='' and cnt<5):
                cnt=5
            elif (guess==kana[quiz] or (eng==True and guess in kana[quiz])):
                score*=2
                print("\nCorrect!\nYour Score is",score)
                cnt=-1
            elif(cnt<5):
                print("\nWrong guess again")
                guess=input()
                cnt+=1
                score=1
            else:
                print("\nSorry,try again\nThe correct answer was\n\t",kana[quiz])
                cnt=-1
                score=1
def table(lib='hk'):
    a=chr(0x3000)
    b=chr(0x2009)
    c=[[' '],['k'],['s'],['t'],['n'],['h'],['m'],['y'],['r'],['w'],[' ']]
    v= ['a','i','u','e','o']
    k=[['ア','イ','ウ','エ','オ'],
       ['カ','キ','ク','ケ','コ'],
       ['サ','シ','ス','セ','ソ'],
       ['タ','チ','ツ','テ','ト'],
       ['ナ','ニ','ヌ','ネ','ノ'],
       ['ハ','ヒ','フ','ヘ','ホ'],
       ['マ','ミ','ム','メ','モ'],
       ['ヤ',a,'ユ',a,'ヨ'],
       ['ラ','リ','ル','レ','ロ'],
       ['ワ',a,a,a,'ヲ'],
       ['ン']]
    h=[['あ','い','う','え','お'],
       ['か','き','く','け','こ'],
       ['さ','し','す','せ','そ'],
       ['た','ち','つ','て','と'],
       ['な','に','ぬ','ね','の'],
       ['は','ひ','ふ','へ','ほ'],
       ['ま','み','む','め','も'],
       ['や',a,'ゆ',a,'よ'],
       ['ら','り','る','れ','ろ'],
       ['わ',a,a,a,'を'],
       ['ん']]
    j=0
    if 'h' in lib:
        print(a,b,b.join(v),sep='')
        for i in c:
            print(i[0],a,''.join(h[j]),sep='')
            j+=1
    if 'k' in lib:
        j=0
        print(a,b,b.join(v),sep='')
        for i in c:
            print(i[0],a,''.join(k[j]),sep='')
            j+=1
    return
if __name__ == '__main__':
    kana_quiz(argv)

