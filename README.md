# Celil Kaan Gungor 201101014 395 Dersi Homework 2
# Simple Calculator Interpreter

Bu proje, coklu programlama dillerinde yazilmis basit bir hesap makinesi yorumlayicisini implement etmektedir.

## Desteklenen Diller

1. Rust
2. ADA
3. Perl
4. Scheme
5. Prolog

## Ozellikler

- Degiskenler ve atamalar
- Temel aritmetik islemler (toplama, cikarma, carpma, bolme)
- Hata yonetimi

## Calistirma

### Rust
```
cd rust
cargo run
```

### ADA
```
cd ada
gnatmake calculator.adb
./calculator
```

### Perl
```
cd perl
perl calculator.pl
```

### Scheme
```
cd scheme
scheme --load calculator.scm
```

### Prolog
```
cd prolog
swipl -s calculator.pl
```

## Girdi Formati

Her bir yorumlayici asagidaki turden ifadeleri kabul eder:
- Aritmetik: `2 + 3 * 4`
- Degisken atamasi: `x = 10`
- Degisken kullanimi: `y = x + 5`

Yorumlayicidan cikmak icin `exit` veya `quit` yaziniz.
