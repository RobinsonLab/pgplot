C*GRSYMK -- convert character number into symbol number
C+
      SUBROUTINE GRSYMK (CODE, FONT, SYMBOL)
      INTEGER CODE, FONT, SYMBOL
C
C This routine returns the Hershey symbol number (SYMBOL) corresponding
C to ASCII code CODE in font FONT.
C
C Characters 0-31 are the same in all fonts, and are the standard
C graph markers. Characters 32-127 are standard representations of
C the ASCII codes. Characters 128-255 are reserved for the upper
C half of the ISO Latin-1 character set. Characters 256-303 are
C used for the greek alphabet.
C
C Arguments:
C  CODE   (input)  : the extended ASCII code number.
C  FONT   (input)  : the font to be used 31 (range 1-4).
C  SYMBOL (output) : the number of the symbol to be plotted.
C--
C 24-Apr-1986.
C 15-Dec-1988 - standardize [TJP].
C 29-Nov-1990 - eliminate common block [TJP].
C 27-Nov-1991 - correct code for backslash [TJP].
C 27-Jul-1995 - extend for 256-character set; add some defaults for
C               ISO Latin-1 (full glyph set not available) [TJP].
C-----------------------------------------------------------------------
      INTEGER   I, K, HERSH(0:303,4)
      SAVE      HERSH
C
C Special characters (graph markers).
C
      DATA (HERSH(  0,K),K=1,4) / 841, 841, 841, 841/
      DATA (HERSH(  1,K),K=1,4) / 899, 899, 899, 899/
      DATA (HERSH(  2,K),K=1,4) / 845, 845, 845, 845/
      DATA (HERSH(  3,K),K=1,4) / 847, 847, 847, 847/
      DATA (HERSH(  4,K),K=1,4) / 840, 840, 840, 840/
      DATA (HERSH(  5,K),K=1,4) / 846, 846, 846, 846/
      DATA (HERSH(  6,K),K=1,4) / 841, 841, 841, 841/
      DATA (HERSH(  7,K),K=1,4) / 842, 842, 842, 842/
      DATA (HERSH(  8,K),K=1,4) /2284,2284,2284,2284/
      DATA (HERSH(  9,K),K=1,4) /2281,2281,2281,2281/
      DATA (HERSH( 10,K),K=1,4) / 735, 735, 735, 735/
      DATA (HERSH( 11,K),K=1,4) / 843, 843, 843, 843/
      DATA (HERSH( 12,K),K=1,4) / 844, 844, 844, 844/
      DATA (HERSH( 13,K),K=1,4) / 852, 852, 852, 852/
      DATA (HERSH( 14,K),K=1,4) / 866, 866, 866, 866/
      DATA (HERSH( 15,K),K=1,4) / 868, 868, 868, 868/
      DATA (HERSH( 16,K),K=1,4) / 851, 851, 851, 851/
      DATA (HERSH( 17,K),K=1,4) / 850, 850, 850, 850/
      DATA (HERSH( 18,K),K=1,4) / 856, 856, 856, 856/
      DATA (HERSH( 19,K),K=1,4) / 254, 254, 254, 254/
      DATA (HERSH( 20,K),K=1,4) / 900, 900, 900, 900/
      DATA (HERSH( 21,K),K=1,4) / 901, 901, 901, 901/
      DATA (HERSH( 22,K),K=1,4) / 902, 902, 902, 902/
      DATA (HERSH( 23,K),K=1,4) / 903, 903, 903, 903/
      DATA (HERSH( 24,K),K=1,4) / 904, 904, 904, 904/
      DATA (HERSH( 25,K),K=1,4) / 905, 905, 905, 905/
      DATA (HERSH( 26,K),K=1,4) / 906, 906, 906, 906/
      DATA (HERSH( 27,K),K=1,4) / 907, 907, 907, 907/
      DATA (HERSH( 28,K),K=1,4) /2263,2263,2263,2263/
      DATA (HERSH( 29,K),K=1,4) /2261,2261,2261,2261/
      DATA (HERSH( 30,K),K=1,4) /2262,2262,2262,2262/
      DATA (HERSH( 31,K),K=1,4) /2264,2264,2264,2264/
C
C US-ASCII (ISO Latin-1 lower half).
C
C   32:39 space exclam quotdbl numbersign
C         dollar percent ampersand quoteright
      DATA (HERSH( 32,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH( 33,K),K=1,4) / 714,2214,2764,2764/
      DATA (HERSH( 34,K),K=1,4) / 717,2217,2778,2778/
      DATA (HERSH( 35,K),K=1,4) / 733,2275,2275,2275/
      DATA (HERSH( 36,K),K=1,4) / 719,2274,2769,2769/
      DATA (HERSH( 37,K),K=1,4) /2271,2271,2271,2271/
      DATA (HERSH( 38,K),K=1,4) / 734,2272,2768,2768/
      DATA (HERSH( 39,K),K=1,4) / 716,2216,2777,2777/
C   40:47 parenleft parenright asterisk plus
C         comma minus period slash
      DATA (HERSH( 40,K),K=1,4) / 721,2221,2771,2771/
      DATA (HERSH( 41,K),K=1,4) / 722,2222,2772,2772/
      DATA (HERSH( 42,K),K=1,4) / 728,2219,2773,2773/
      DATA (HERSH( 43,K),K=1,4) / 725,2232,2775,2775/
      DATA (HERSH( 44,K),K=1,4) / 711,2211,2761,2761/
      DATA (HERSH( 45,K),K=1,4) / 724,2231,2774,2774/
      DATA (HERSH( 46,K),K=1,4) / 710,2210,2760,2760/
      DATA (HERSH( 47,K),K=1,4) / 720,2220,2770,2770/
C   48:55 zero one two three four five six seven
      DATA (HERSH( 48,K),K=1,4) / 700,2200,2750,2750/
      DATA (HERSH( 49,K),K=1,4) / 701,2201,2751,2751/
      DATA (HERSH( 50,K),K=1,4) / 702,2202,2752,2752/
      DATA (HERSH( 51,K),K=1,4) / 703,2203,2753,2753/
      DATA (HERSH( 52,K),K=1,4) / 704,2204,2754,2754/
      DATA (HERSH( 53,K),K=1,4) / 705,2205,2755,2755/
      DATA (HERSH( 54,K),K=1,4) / 706,2206,2756,2756/
      DATA (HERSH( 55,K),K=1,4) / 707,2207,2757,2757/
C   56:63 eight nine colon semicolon less equal greater question
      DATA (HERSH( 56,K),K=1,4) / 708,2208,2758,2758/
      DATA (HERSH( 57,K),K=1,4) / 709,2209,2759,2759/
      DATA (HERSH( 58,K),K=1,4) / 712,2212,2762,2762/
      DATA (HERSH( 59,K),K=1,4) / 713,2213,2763,2763/
      DATA (HERSH( 60,K),K=1,4) /2241,2241,2241,2241/
      DATA (HERSH( 61,K),K=1,4) / 726,2238,2776,2776/
      DATA (HERSH( 62,K),K=1,4) /2242,2242,2242,2242/
      DATA (HERSH( 63,K),K=1,4) / 715,2215,2765,2765/
C   64:71 at A B C D E F G
      DATA (HERSH( 64,K),K=1,4) /2273,2273,2273,2273/
      DATA (HERSH( 65,K),K=1,4) / 501,2001,2051,2551/
      DATA (HERSH( 66,K),K=1,4) / 502,2002,2052,2552/
      DATA (HERSH( 67,K),K=1,4) / 503,2003,2053,2553/
      DATA (HERSH( 68,K),K=1,4) / 504,2004,2054,2554/
      DATA (HERSH( 69,K),K=1,4) / 505,2005,2055,2555/
      DATA (HERSH( 70,K),K=1,4) / 506,2006,2056,2556/
      DATA (HERSH( 71,K),K=1,4) / 507,2007,2057,2557/
C   72:79 H I J K L M N O
      DATA (HERSH( 72,K),K=1,4) / 508,2008,2058,2558/
      DATA (HERSH( 73,K),K=1,4) / 509,2009,2059,2559/
      DATA (HERSH( 74,K),K=1,4) / 510,2010,2060,2560/
      DATA (HERSH( 75,K),K=1,4) / 511,2011,2061,2561/
      DATA (HERSH( 76,K),K=1,4) / 512,2012,2062,2562/
      DATA (HERSH( 77,K),K=1,4) / 513,2013,2063,2563/
      DATA (HERSH( 78,K),K=1,4) / 514,2014,2064,2564/
      DATA (HERSH( 79,K),K=1,4) / 515,2015,2065,2565/
C   80:87 P Q R S T U V W
      DATA (HERSH( 80,K),K=1,4) / 516,2016,2066,2566/
      DATA (HERSH( 81,K),K=1,4) / 517,2017,2067,2567/
      DATA (HERSH( 82,K),K=1,4) / 518,2018,2068,2568/
      DATA (HERSH( 83,K),K=1,4) / 519,2019,2069,2569/
      DATA (HERSH( 84,K),K=1,4) / 520,2020,2070,2570/
      DATA (HERSH( 85,K),K=1,4) / 521,2021,2071,2571/
      DATA (HERSH( 86,K),K=1,4) / 522,2022,2072,2572/
      DATA (HERSH( 87,K),K=1,4) / 523,2023,2073,2573/
C   88:95 X Y Z bracketleft 
C         backslash bracketright asciicircum underscore
      DATA (HERSH( 88,K),K=1,4) / 524,2024,2074,2574/
      DATA (HERSH( 89,K),K=1,4) / 525,2025,2075,2575/
      DATA (HERSH( 90,K),K=1,4) / 526,2026,2076,2576/
      DATA (HERSH( 91,K),K=1,4) /2223,2223,2223,2223/
      DATA (HERSH( 92,K),K=1,4) / 804, 804, 804, 804/
      DATA (HERSH( 93,K),K=1,4) /2224,2224,2224,2224/
      DATA (HERSH( 94,K),K=1,4) / 718,2218,2779,2779/
      DATA (HERSH( 95,K),K=1,4) / 590, 590, 590, 590/
C   96:103 quoteleft a b c d e f g
      DATA (HERSH( 96,K),K=1,4) /2249,2249,2249,2249/
      DATA (HERSH( 97,K),K=1,4) / 601,2101,2151,2651/
      DATA (HERSH( 98,K),K=1,4) / 602,2102,2152,2652/
      DATA (HERSH( 99,K),K=1,4) / 603,2103,2153,2653/
      DATA (HERSH(100,K),K=1,4) / 604,2104,2154,2654/
      DATA (HERSH(101,K),K=1,4) / 605,2105,2155,2655/
      DATA (HERSH(102,K),K=1,4) / 606,2106,2156,2656/
      DATA (HERSH(103,K),K=1,4) / 607,2107,2157,2657/
C  104:111 h i j k l m n o
      DATA (HERSH(104,K),K=1,4) / 608,2108,2158,2658/
      DATA (HERSH(105,K),K=1,4) / 609,2109,2159,2659/
      DATA (HERSH(106,K),K=1,4) / 610,2110,2160,2660/
      DATA (HERSH(107,K),K=1,4) / 611,2111,2161,2661/
      DATA (HERSH(108,K),K=1,4) / 612,2112,2162,2662/
      DATA (HERSH(109,K),K=1,4) / 613,2113,2163,2663/
      DATA (HERSH(110,K),K=1,4) / 614,2114,2164,2664/
      DATA (HERSH(111,K),K=1,4) / 615,2115,2165,2665/
C  112:119 p q r s t u v w
      DATA (HERSH(112,K),K=1,4) / 616,2116,2166,2666/
      DATA (HERSH(113,K),K=1,4) / 617,2117,2167,2667/
      DATA (HERSH(114,K),K=1,4) / 618,2118,2168,2668/
      DATA (HERSH(115,K),K=1,4) / 619,2119,2169,2669/
      DATA (HERSH(116,K),K=1,4) / 620,2120,2170,2670/
      DATA (HERSH(117,K),K=1,4) / 621,2121,2171,2671/
      DATA (HERSH(118,K),K=1,4) / 622,2122,2172,2672/
      DATA (HERSH(119,K),K=1,4) / 623,2123,2173,2673/
C  120:127 x y z braceleft bar braceright asciitilde -
      DATA (HERSH(120,K),K=1,4) / 624,2124,2174,2674/
      DATA (HERSH(121,K),K=1,4) / 625,2125,2175,2675/
      DATA (HERSH(122,K),K=1,4) / 626,2126,2176,2676/
      DATA (HERSH(123,K),K=1,4) /2225,2225,2225,2225/
      DATA (HERSH(124,K),K=1,4) / 723,2229,2229,2229/
      DATA (HERSH(125,K),K=1,4) /2226,2226,2226,2226/
      DATA (HERSH(126,K),K=1,4) /2246,2246,2246,2246/
      DATA (HERSH(127,K),K=1,4) / 699,2199,2199,2199/
C
C ISO Latin-1 upper half.
C
C  128:135 - - - - - - - -
      DATA (HERSH(128,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(129,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(130,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(131,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(132,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(133,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(134,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(135,K),K=1,4) / 699,2199,2199,2199/
C  136:143 - - - - - - - -
      DATA (HERSH(136,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(137,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(138,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(139,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(140,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(141,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(142,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(143,K),K=1,4) / 699,2199,2199,2199/
C   144:151 dotlessi grave acute circumflex tilde - breve dotaccent
      DATA (HERSH(144,K),K=1,4) / 699,2182,2196,2199/
      DATA (HERSH(145,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(146,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(147,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(148,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(149,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(150,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(151,K),K=1,4) / 699,2199,2199,2199/
C   152:159 dieresis - ring - - - - -
      DATA (HERSH(152,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(153,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(154,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(155,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(156,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(157,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(158,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(159,K),K=1,4) / 699,2199,2199,2199/
C   160:167 space exclamdown cent sterling currency yen brokenbar section
      DATA (HERSH(160,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(161,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(162,K),K=1,4) / 910, 910, 910, 910/
      DATA (HERSH(163,K),K=1,4) / 272, 272, 272, 272/
      DATA (HERSH(164,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(165,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(166,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(167,K),K=1,4) /2276,2276,2276,2276/
C   168:175 - copyright - - - - registered -
      DATA (HERSH(168,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(169,K),K=1,4) / 274, 274, 274, 274/
      DATA (HERSH(170,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(171,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(172,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(173,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(174,K),K=1,4) / 273, 273, 273, 273/
      DATA (HERSH(175,K),K=1,4) / 699,2199,2199,2199/
C   176:183 degree plusminus twosuperior threesuperior
C           acute mu paragraph periodcentered
      DATA (HERSH(176,K),K=1,4) / 718,2218,2779,2779/
      DATA (HERSH(177,K),K=1,4) /2233,2233,2233,2233/
      DATA (HERSH(178,K),K=1,4) / 702,2202,2752,2752/
      DATA (HERSH(179,K),K=1,4) / 703,2203,2753,2753/
      DATA (HERSH(180,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(181,K),K=1,4) / 638,2138,2138,2138/
      DATA (HERSH(182,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(183,K),K=1,4) / 729, 729, 729, 729/
C   184:191 cedilla onesuperior ordmasculine guillemotright
C           onequarter onehalf threequarters questiondown
      DATA (HERSH(184,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(185,K),K=1,4) / 701,2201,2751,2751/
      DATA (HERSH(186,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(187,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(188,K),K=1,4) / 270, 270, 270, 270/
      DATA (HERSH(189,K),K=1,4) / 261, 261, 261, 261/
      DATA (HERSH(190,K),K=1,4) / 271, 271, 271, 271/
      DATA (HERSH(191,K),K=1,4) / 699,2199,2199,2199/
C   192:199 Agrave Aacute Acircumflex Atilde Aring AE Ccedilla
      DATA (HERSH(192,K),K=1,4) / 501,2001,2051,2551/
      DATA (HERSH(193,K),K=1,4) / 501,2001,2051,2551/
      DATA (HERSH(194,K),K=1,4) / 501,2001,2051,2551/
      DATA (HERSH(195,K),K=1,4) / 501,2001,2051,2551/
      DATA (HERSH(196,K),K=1,4) / 501,2001,2051,2551/
      DATA (HERSH(197,K),K=1,4) / 501,2078,2051,2551/
      DATA (HERSH(198,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(199,K),K=1,4) / 503,2003,2053,2553/
C   200:207 Egrave Eacute Ecircumflex Edieresis 
C           Igrave Iacute Icircumflex Idieresis
      DATA (HERSH(200,K),K=1,4) / 505,2005,2055,2555/
      DATA (HERSH(201,K),K=1,4) / 505,2005,2055,2555/
      DATA (HERSH(202,K),K=1,4) / 505,2005,2055,2555/
      DATA (HERSH(203,K),K=1,4) / 505,2005,2055,2555/
      DATA (HERSH(204,K),K=1,4) / 509,2009,2059,2559/
      DATA (HERSH(205,K),K=1,4) / 509,2009,2059,2559/
      DATA (HERSH(206,K),K=1,4) / 509,2009,2059,2559/
      DATA (HERSH(207,K),K=1,4) / 509,2009,2059,2559/
C   208:215 Eth Ntilde Ograve Oacute 
C           Ocircumflex Otilde Odieresis multiply
      DATA (HERSH(208,K),K=1,4) / 504,2004,2054,2554/
      DATA (HERSH(209,K),K=1,4) / 514,2014,2064,2564/
      DATA (HERSH(210,K),K=1,4) / 515,2015,2065,2565/
      DATA (HERSH(211,K),K=1,4) / 515,2015,2065,2565/
      DATA (HERSH(212,K),K=1,4) / 515,2015,2065,2565/
      DATA (HERSH(213,K),K=1,4) / 515,2015,2065,2565/
      DATA (HERSH(214,K),K=1,4) / 515,2015,2065,2565/
      DATA (HERSH(215,K),K=1,4) /2235,2235,2235,2235/
C   216:223 Oslash Ugrave Uacute Ucircumflex
C           Udieresis Yacute Thorn germandbls
      DATA (HERSH(216,K),K=1,4) / 515,2015,2065,2565/
      DATA (HERSH(217,K),K=1,4) / 521,2021,2071,2571/
      DATA (HERSH(218,K),K=1,4) / 521,2021,2071,2571/
      DATA (HERSH(219,K),K=1,4) / 521,2021,2071,2571/
      DATA (HERSH(220,K),K=1,4) / 521,2021,2071,2571/
      DATA (HERSH(221,K),K=1,4) / 525,2025,2075,2575/
      DATA (HERSH(222,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(223,K),K=1,4) / 699,2199,2199,2199/
C   224:231 agrave aacute acircumflex atilde aring ae ccedilla
      DATA (HERSH(224,K),K=1,4) / 601,2101,2151,2651/
      DATA (HERSH(225,K),K=1,4) / 601,2101,2151,2651/
      DATA (HERSH(226,K),K=1,4) / 601,2101,2151,2651/
      DATA (HERSH(227,K),K=1,4) / 601,2101,2151,2651/
      DATA (HERSH(228,K),K=1,4) / 601,2101,2151,2651/
      DATA (HERSH(229,K),K=1,4) / 601,2101,2151,2651/
      DATA (HERSH(230,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(231,K),K=1,4) / 603,2103,2153,2653/
C   232:239 egrave eacute ecircumflex edieresis 
C           igrave iacute icircumflex idieresis
      DATA (HERSH(232,K),K=1,4) / 605,2105,2155,2655/
      DATA (HERSH(233,K),K=1,4) / 605,2105,2155,2655/
      DATA (HERSH(234,K),K=1,4) / 605,2105,2155,2655/
      DATA (HERSH(235,K),K=1,4) / 605,2105,2155,2655/
      DATA (HERSH(236,K),K=1,4) / 609,2109,2159,2659/
      DATA (HERSH(237,K),K=1,4) / 609,2109,2159,2659/
      DATA (HERSH(238,K),K=1,4) / 609,2109,2159,2659/
      DATA (HERSH(239,K),K=1,4) / 609,2109,2159,2659/
C   240:247 eth ntilde ograve oacute 
C           ocircumflex otilde odieresis divide
      DATA (HERSH(240,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(241,K),K=1,4) / 614,2114,2164,2664/
      DATA (HERSH(242,K),K=1,4) / 615,2115,2165,2665/
      DATA (HERSH(243,K),K=1,4) / 615,2115,2165,2665/
      DATA (HERSH(244,K),K=1,4) / 615,2115,2165,2665/
      DATA (HERSH(245,K),K=1,4) / 615,2115,2165,2665/
      DATA (HERSH(246,K),K=1,4) / 615,2115,2165,2665/
      DATA (HERSH(247,K),K=1,4) /2237,2237,2237,2237/
C   248:255 oslash ugrave uacute ucircumflex
C           udieresis yacute thorn ydieresis
      DATA (HERSH(248,K),K=1,4) / 615,2115,2165,2665/
      DATA (HERSH(249,K),K=1,4) / 621,2121,2171,2671/
      DATA (HERSH(250,K),K=1,4) / 621,2121,2171,2671/
      DATA (HERSH(251,K),K=1,4) / 621,2121,2171,2671/
      DATA (HERSH(252,K),K=1,4) / 621,2121,2171,2671/
      DATA (HERSH(253,K),K=1,4) / 625,2125,2175,2675/
      DATA (HERSH(254,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(255,K),K=1,4) / 625,2125,2175,2675/
C
C Greek alphabet.
C
      DATA (HERSH(256,K),K=1,4) / 527,2027,2027,2027/
      DATA (HERSH(257,K),K=1,4) / 528,2028,2028,2028/
      DATA (HERSH(258,K),K=1,4) / 529,2029,2029,2029/
      DATA (HERSH(259,K),K=1,4) / 530,2030,2030,2030/
      DATA (HERSH(260,K),K=1,4) / 531,2031,2031,2031/
      DATA (HERSH(261,K),K=1,4) / 532,2032,2032,2032/
      DATA (HERSH(262,K),K=1,4) / 533,2033,2033,2033/
      DATA (HERSH(263,K),K=1,4) / 534,2034,2034,2034/
      DATA (HERSH(264,K),K=1,4) / 535,2035,2035,2035/
      DATA (HERSH(265,K),K=1,4) / 536,2036,2036,2036/
      DATA (HERSH(266,K),K=1,4) / 537,2037,2037,2037/
      DATA (HERSH(267,K),K=1,4) / 538,2038,2038,2038/
      DATA (HERSH(268,K),K=1,4) / 539,2039,2039,2039/
      DATA (HERSH(269,K),K=1,4) / 540,2040,2040,2040/
      DATA (HERSH(270,K),K=1,4) / 541,2041,2041,2041/
      DATA (HERSH(271,K),K=1,4) / 542,2042,2042,2042/
      DATA (HERSH(272,K),K=1,4) / 543,2043,2043,2043/
      DATA (HERSH(273,K),K=1,4) / 544,2044,2044,2044/
      DATA (HERSH(274,K),K=1,4) / 545,2045,2045,2045/
      DATA (HERSH(275,K),K=1,4) / 546,2046,2046,2046/
      DATA (HERSH(276,K),K=1,4) / 547,2047,2047,2047/
      DATA (HERSH(277,K),K=1,4) / 548,2048,2048,2048/
      DATA (HERSH(278,K),K=1,4) / 549,2049,2049,2049/
      DATA (HERSH(279,K),K=1,4) / 550,2050,2050,2050/
      DATA (HERSH(280,K),K=1,4) / 627,2127,2127,2127/
      DATA (HERSH(281,K),K=1,4) / 628,2128,2128,2128/
      DATA (HERSH(282,K),K=1,4) / 629,2129,2129,2129/
      DATA (HERSH(283,K),K=1,4) / 630,2130,2130,2130/
      DATA (HERSH(284,K),K=1,4) / 684,2184,2184,2184/
      DATA (HERSH(285,K),K=1,4) / 632,2132,2132,2132/
      DATA (HERSH(286,K),K=1,4) / 633,2133,2133,2133/
      DATA (HERSH(287,K),K=1,4) / 685,2185,2185,2185/
      DATA (HERSH(288,K),K=1,4) / 635,2135,2135,2135/
      DATA (HERSH(289,K),K=1,4) / 636,2136,2136,2136/
      DATA (HERSH(290,K),K=1,4) / 637,2137,2137,2137/
      DATA (HERSH(291,K),K=1,4) / 638,2138,2138,2138/
      DATA (HERSH(292,K),K=1,4) / 639,2139,2139,2139/
      DATA (HERSH(293,K),K=1,4) / 640,2140,2140,2140/
      DATA (HERSH(294,K),K=1,4) / 641,2141,2141,2141/
      DATA (HERSH(295,K),K=1,4) / 642,2142,2142,2142/
      DATA (HERSH(296,K),K=1,4) / 643,2143,2143,2143/
      DATA (HERSH(297,K),K=1,4) / 644,2144,2144,2144/
      DATA (HERSH(298,K),K=1,4) / 645,2145,2145,2145/
      DATA (HERSH(299,K),K=1,4) / 646,2146,2146,2146/
      DATA (HERSH(300,K),K=1,4) / 686,2186,2186,2186/
      DATA (HERSH(301,K),K=1,4) / 648,2148,2148,2148/
      DATA (HERSH(302,K),K=1,4) / 649,2149,2149,2149/
      DATA (HERSH(303,K),K=1,4) / 650,2150,2150,2150/
C
      IF ((CODE.LT.0) .OR. (CODE.GT.303)) THEN
          I = 1
      ELSE
          I = CODE
      END IF
      SYMBOL = HERSH(I,FONT)
C
      END
