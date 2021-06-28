C     PROGRAM FREE STOCHASTIC MUSIC (FORTRAN IV)                        XEN    6
C                                                                       XEN    7
C     GLOSSARY OF THE PRINCIPAL ABBREVIATIONS                           XEN    8
C
C     A - DURATION OF EACH SEQUENCE IN SECONDS                          XEN    9
C     A10,A20,A17,A35,A30 - NUMBERS FOR GLISSANDO CALCULATION           XEN   10
C     ALEA - PARAMETER USED TO ALTER THE RESULT OF A SECOND RUN WITH THEXEN   11
C     SAME INPUT DATA                                                   XEN   12
C     ALFA(3) - THREE EXPRESSIONS ENTERING INTO THE THREE SPEED VALUES  XEN   13
C     OF THE SLIDING TONES ( GLISSANDI )                                XEN   14
C     ALIM - MAXIMUM LIMIT OF SEQUENCE DURATION A                       XEN   15
C     (AMAX(I),I=1,KTR) TABLE OF AN EXPRESSION ENTERING INTO THE        XEN   16
C     CALCULATION OF THE NOTE LENGTH IN PART 8                          XEN   17
C     BF - DYNAMIC FORM NUMBER, THE LIST IS ESTABLISHED INDEPENDENTLY   XEN   18
C     OF THIS PROGRAM AND IS SUBJECT TO MODIFICATION                    XEN   19
C     DELTA - THE RECIPROCAL OF THE MEAN DENSITY OF SOUND EVENTS DURING XEN   20
C     A SEQUENCE OF DURATION A                                          XEN   21
C     (E(I,J),I=1,KTR,J=1,KTE) - PROBABILITIES OF THE KTR TIMBRE CLASSESXEN   22
C     INTRODUCED AS INPUT DATA, DEPENDING ON THE CLASS NUMBER I=KR      XEN   23
C     AND ON THE POWER J=U OBTAINED FROM V3*EXPF(U)=DA                  XEN   24
C     EPSI - EPSILON FOR ACCURACY IN CALCULATING PN AND E(I,J),WHICH    XEN   25
C     IT IS ADVISABLE TO RETAIN.                                        XEN   26
C     (GN(I,J),I=1,KTR,J=1,KTS) - TABLE OF THE GIVEN LENGTH OF BREATH   XEN   27
C     FOR EACH INSTRUMENT, DEPENDING ON CLASS I AND INSTRUMENT J        XEN   28
C     GTNA - GREATEST NUMBER OF NOTES IN THE SEQUENCE OF DURATION A     XEN   29
C     GTNS - GREATEST NUMBER OF NOTES IN KW LOOPS                       XEN   30
C     (HAMIN(I,J),HAMAX(I,J),HBMIN(I,J),HBMAX(I,J),I=1,KTR,J=1,KTS)     XEN   31
C     TABLE OF INSTRUMENT COMPASS LIMITS, DEPENDING ON TIMBRE CLASS I   XEN   32
C     AND INSTRUMENT J. TEST INSTRUCTION 480 IN PART 6 DETERMINES       XEN   33
C     WHETHER THE HA OR THE HB TABLE IS FOLLOWED. THE NUMBER 7 IS       XEN   34
C     ARBITRARY.                                                        XEN   35
C     JW - ORDINAL NUMBER OF THE SEQUENCE COMPUTED.                     XEN   36
C     KNL - NUMBER OF LINES PER PAGE OF THE PRINTED RESULT.KNL=50       XEN   37
C     KR1 - NUMBER IN THE CLASS KR=1 USED FOR PERCUSSION OR INSTRUMENTS XEN   38
C     WITHOUT A DEFINITE PITCH.                                         XEN   39
C     KTE - POWER OF THE EXPONENTIAL COEFFICIENT E SUCH THAT            XEN   40
C     DA(MAX)=V3*(E**(KTE-1))                                           XEN   41
C     KTR - NUMBER OF TIMBRE CLASSES                                    XEN   42
C     KW - MAXIMUM NUMBER OF JW                                         XEN   43
C     KTEST1,TAV1,ETC. - EXPRESSIONS USEFUL IN CALCULATING HOW LONG THE XEN   44
C     VARIOUS PARTS OF THE PROGRAM WILL RUN.                            XEN   45
C     KT1 - ZERO IF THE PROGRAM IS BEING RUN, NONZERO DURING DEBUGGING  XEN   46
C     KT2 - NUMBER OF LOOPS, EQUAL TO 15 BY ARBITRARY DEFINITION.       XEN   47
C     (MODI(IX8),IX8=7,1) AUXILIARY FUNCTION TO INTERPOLATE VALUES IN   XEN   48
C     THE TETA(256) TABLE (SEE PART 7)                                  XEN   49
C     NA - NUMBER OF SOUNDS CALCULATED FOR THE SEQUENCE A (NA=DA*A)     XEN   50
C     (NT(I),I=1,KTR) NUMBER OF INSTRUMENTS ALLOCATED TO EACH OF THE    XEN   51
C     KTR TIMBRE CLASSES.                                               XEN   52
C     (PN(I,J),I=1,KTR,J=1,KTS),(KTS=NT(I),I=1,KTR) TABLE OF PROBABILITYXEN   53
C     OF EACH INSTRUMENT OF THE CLASS I.                                XEN   54
C     (Q(I),I=1,KTR) PROBABILITIES OF THE KTR TIMBRE CLASSES, CONSIDEREDXEN   55
C     AS LINEAR FUNCTIONS OF THE DENSITY DA.                            XEN   56
C     (S(I),I=1,KTR) SUN OF THE SUCCESSIVE Q(I) PROBABILITIES, USED TO  XEN   57
C     CHOOSE THE CLASS KR BY COMPARING IT TO A RANDOM NUMBER X1 (SEE    XEN   58
C     PART 3, LOOP 380 AND PART 5, LOOP 430).                           XEN   59
C     SINA - SUM OF THE COMPUTED NOTES IN THE NEW CLOUDS NA, ALWAYS LESSXEN   60
C     THAN GTNS ( SEE TEST IN PART 10 ).                                XEN   61
C     SQPI - SQUARE ROOT OF PI (3.14159...)                             XEN   62
C     TA - SOUND ATTACK TIME ABCISSA.                                   XEN   63
C     TETA(256) - TABLE OF THE 256 VALUES OF TEH INTEGRAL OF THE NORMAL XEN   64
C     DISTRIBUTION CURVE WHICH IS USEFUL IN CALCULATING GLISSANDO SPEED XEN   65
C     AND SOUND EVENT DURATION.                                         XEN   66
C     VIGL - GLISSANDO SPEED (VITESSE GLISSANDO), WHICH CAN VARY AS, BE XEN   67
C     INDEPENDENT OF, OR VARY INVERSELY AS THE DENSITY OF THE SEQUENCE, XEN   68
C     THE ACTUAL MODE OF VARIATION EMPLOYED REMAINING THE SAME FOR THE  XEN   69
C     ENTIRE SEQUENCE (SEE PART 7).                                     XEN   70
C     VITLIM - MAXIMUM LIMITING GLISSANDO SPEED (IN SEMITONES/SEC),     XEN   71
C     SUBJECT TO MODIFICATION.                                          XEN   72
C     V3 - MINIMUM CLOUD DENSITY DA                                     XEN   73
C     (Z1(I),Z2(I),I=1,8) TABLE COMPLEMENTARY TO THE TETA TABLE.        XEN   74
C                                                                       XEN   75
C                                                                       XEN   76
C     READ CONSTANTS AND TABLES                                         XEN   78
C                                                                       XEN   77
      DIMENSION Q(12),S(12),E(12,12),PN(12,50),SPN(12,50),NT(12),       XEN   79
     1HAMIN(12,50),HAMAX(12,50),HBMIN(12,50),HBMAX(12,50),GN(12,50),H(12XEN   80
     2,50),TETA(256),VIGL(3),MODI(7),Z1(8),Z2(8),ALFA(3),AMAX(12)       XEN   81
C                                                                       XEN   82
C                                                                       XEN   83
C                                                                       XEN   84
      I=1                                                               XEN   85
      DO 10 IX=1,7                                                      XEN   86
      IX8=8-IX                                                          XEN   87
      MODI(IX8)=I                                                       XEN   88
 10   I=I+1                                                             XEN   89
C                                             	                        XEN   90
      READ 20,(TETA(I),I=1,256)                                         XEN   91
 20   FORMAT(12F6.6)                                                    XEN   92
      READ 30,(Z1(I),Z2(I),I=1,8)                                       XEN   93
 30   FORMAT(6(F3.2,F9.8)/F3.2,F9.8,E6.2,F9.8)                          XEN   94
      PRINT 40,TETA,Z1,Z2                                               XEN   95
 40   FORMAT('1  THE TETA TABLE = ',/,21(12F10.6,/),4F10.6,/////,       XEN   96
     1' THE Z1 TABLE = ',/,7F6.2,E12.3,///,' THE Z2 TABLE = ',/,8F14.8,/XEN   97
     11H1)                                                              XEN   98
      READ 50,DELTA,V3,A10,A20,A17,A30,A35,BF,SQPI,EPSI,VITLIM,ALEA,   AXEN   99
     1LIM                                                               XEN  100
 50   FORMAT(F3.0,F3.3,5F3.1,F2.0,F8.7,F8.8,F4.2,F8.8,F5.2)             XEN  114
      READ 60,KT1,KT2,KW,KNL,KTR,KTE,KR1,GTNA,GTNS,(NT(I),I=1,KTR)      XEN  115
 60   FORMAT(5I3,2I2,2F6.0,12I2)                                        XEN  126
      PRINT 70,DELTA,V3,A10,A20,A17,A30,A35,BF,SQPI,EPSI,VITLIM,ALEA,  AXEN  127
     1LIM,KT1,KT2,KW,KNL,KTR,KTE,KR1,GTNA,GTNS,(I,NT(I),I=1,KTR)        XEN  128
 70   FORMAT('1DELTA = ',F4.0,/,' V3 = ',F6.3,/,' A10 = ',F4.1,/,       XEN  129
     1' A20 = ',F4.1,/,' A17 = ',F4.1,/,' A30 = ',F4.1,/,' A35 = ',F4.1,XEN  130
     2/,' BF = ',F3.0,/,' SQPI =',F11.8,/,' EPSI =',F12.8,/,' VITLIM = 'XEN  131
     3,F5.2,/,' ALEA =',F12.8,/,' ALIM = ',F6.2,/,' KT1 = ',I3,/,       XEN  132
     4' KT2 = ',I3,/,' KW = ',I3,/,' KNL = ',I3,/,' KTR = ',I3,/,       XEN  133
     5' KTE = ',I2,/,' KR1 = ',I2,/,' GTNA = ',F7.0,/,' GTNS = ',F7.0,  XEN  134
     6/,12(' IN CLASS ',I2,', THERE ARE ',I2,' INSTRUMENTS.',/))        XEN  135
      READ 80,KTEST3,KTEST1,KTEST2                                      XEN  136
 80   FORMAT(5I3)                                                       XEN  141
      PRINT 90,KTEST3,KTEST1,KTEST2                                     XEN  142
 90   FORMAT(' KTEST3 = ',I3,/,' KTEST1 = ',I3,/,' KTEST2 = ',I3)       XEN  143
C                                                                       XEN  144
      IF(KTEST3.NE.0) PRINT 830                                         XEN  145
      R=KTE-1                                                           XEN  146
      A10=A10*SQPI                                                      XEN  147
      A20=A20*SQPI/R                                                    XEN  148
      A30=A30*SQPI                                                      XEN  149
C     IF ALEA IS NON-ZERO, THE RANDOM NUMBER IS GENERATED FROM THE TIME XEN  150
C     WHEN THE FOLLOWING INSTRUCTION IS EXECUTED. IF ALEA IS NON-ZERO   XEN  151
C     EACH RUN OF THIS PROGRAM WILL PRODUCE DIFFERENT OUTPUT DATA.      XEN  152
      IF(ALEA.NE.0.0) CALL RANFSET(TIMEF(1))                            XEN  153
      PRINT 830                                                         XEN  154
      DO 130 I=1,KTR                                                    XEN  155
      Y=0.0                                                             XEN  156
      KTS=NT(I)                                                         XEN  157
      READ 100,(HAMIN(I,J),HAMAX(I,J),HBMIN(I,J),HBMAX(I,J),GN(I,J),    XEN  158
     1PN(I,J),J=1,KTS)                                                  XEN  159
 100  FORMAT(5(5F2.0,F3.3))                                             XEN  160
      PRINT 110,I,(J,HAMIN(I,J),HAMAX(I,J),HBMIN(I,J),HBMAX(I,J),GN(I,J)XEN  161
     1,PN(I,J),J=1,KTS)                                                 XEN  162
 110  FORMAT(////,' IN CLASS NUMBER ',I2,/,(' FOR INSTRUMENT NO. ',I2,  XEN  163
     1' HAMIN = ',F3.0,',HAMAX = ',F3.0,',HBMIN = ',F3.0,',HBMAX = ',   XEN  164
     2 F3.0,',GN = ',F3.0,', AND PN = ',F6.3))                          XEN  165
      DO 120 J=1,KTS                                                    XEN  166
      Y=Y+PN(I,J)                                                       XEN  167
 120  SPN(I,J)=Y                                                        XEN  168
 130  IF(ABSF(Y-1.0).GE.EPSI) CALL EXIT                                 XEN  169
C                                                                       XEN  170
      DO 150 I=1,KTR                                                    XEN  171
      READ 140,(E(I,J),J=1,KTE)                                         XEN  172
 140  FORMAT(12F2.2)                                                    XEN  173
 150  PRINT 160,I,(J,E(I,J),J=1,KTE)                                    XEN  174
 160  FORMAT(//////,' CLASS NUMBER ',I2,/,(' IN DENSITY LEVEL ',I2,     XEN  175
     1' HAS A PROBABILITY OF ',F6.2))                                   XEN  176
      DO 180 J=1,KTE                                                    XEN  177
      Y=0.0                                                             XEN  178
      DO 170 I=1,KTR                                                    XEN  179
 170  Y=Y+E(I,J)                                                        XEN  180
 180  IF(ABSF(Y-1.0).GE.EPSI) CALL EXIT                                 XEN  181
      DO 200 I=1,KTR                                                    XEN  182
      AMAX(I)=1.0/E(I,1)                                                XEN  183
      DO 200 J=2,KTE                                                    XEN  184
      AJ=J-1                                                            XEN  185
      AX=1.0/(E(I,J)*EXPF(AJ))                                          XEN  186
      IF (KT1.NE.0) PRINT 190,AX                                        XEN  187
 190  FORMAT(1H ,9E12.8)                                                XEN  188
 200  IF(AX.GT.AMAX(I)) AMAX(I)=AX                                      XEN  189
      IF(KT1.NE.0) WRITE(2,210) AMAX                                    XEN  190
 210  FORMAT(6E15.8)                                                    XEN  191
C                                                                       XEN  192
      JW=1                                                              XEN  193
      SINA=0.0                                                          XEN  194
      IF(KTEST1.NE.0) TAV1=TIMEF(1)                                     XEN  195
 220  NLINE=50                                                          XEN  196
C                                                                       XEN  197
C     PARTS 1 AND 2, DEFINE SEQUENCE A SECONDS AND CLOUD NA DURING A    XEN  198
C                                                                       XEN  199
      KNA=0                                                             XEN  200
      K1=0                                                              XEN  201
 230  X1=RANF(-1)                                                       XEN  202
      A=-DELTA * LOGF(X1)                                               XEN  203
      IF(A.LE.ALIM) GO TO 250                                           XEN  204
      IF(K1.GE.KT2) GO TO 240                                           XEN  205
      K1=K1+1                                                           XEN  206
      GO TO 230                                                         XEN  207
 240  A=ALIM/2.0                                                        XEN  208
      X1=0.0                                                            XEN  209
 250  K2=0                                                              XEN  210
 260  X2=RANF(-1)                                                       XEN  211
      IF(JW.GT.1) GO TO 280                                             XEN  212
 270  UX=R*X2                                                           XEN  213
      GO TO 310                                                         XEN  214
 280  IF(RANF(-1).GE.0.5) GO TO 290                                     XEN  215
      UX=UPR + R * (1.0-SQRTF(X2))                                      XEN  216
      GO TO 300                                                         XEN  217
 290  UX=UPR - R * (1.0-SQRTF(X2))                                      XEN  218
 300  IF((UX.GE.0.0).AND.(UX.LE.R)) GO TO 310                           XEN  219
      IF(K2.GE.KT2) GO TO 270                                           XEN  220
      K2=K2+1                                                           XEN  221
      GO TO 260                                                         XEN  222
 310  U=UX                                                              XEN  223
      DA=V3*EXPF(U)                                                     XEN  224
      NA=XINTF(A * DA + 0.5) + 1                                        XEN  225
      IF (GTNA.GT.FLOATF(NA)) GO TO 330                                 XEN  226
      IF (KNA.GE.KT2) GO TO 320                                         XEN  227
      KNA=KNA+1                                                         XEN  228
      GO TO 230                                                         XEN  229
 320  A=DELTA                                                           XEN  230
      GO TO 260                                                         XEN  231
 330  UPR=U                                                             XEN  232
      IF (KT1.EQ.0) GO TO 360                                           XEN  233
      PRINT 340,JW,KNA,K1,K2,X1,X2,A,DA,NA                              XEN  234
 340  FORMAT(1H1,4I8,3X,4E18.8,3X,I8)                                   XEN  235
      NA=KT1                                                            XEN  236
      IF (KTEST3.NE.0) PRINT 350,JW,NA,A                                XEN  237
 350  FORMAT(1H0,2I9,F10.2)                                             XEN  238
C                                                                       XEN  239
C     PART 3. DEFINE CONSTITUTION OF ORCHESTRA DURING SEQUENCE A        XEN  240
C                                                                       XEN  241
 360  SINA=SINA + FLOATF(NA)                                            XEN  242
      XLOGDA=U                                                          XEN  243
      XALOG=A20 *XLOGDA                                                 XEN  244
      M=XINTF(XLOGDA)                                                   XEN  245
      IF ((M+2).GT.KTE) M=KTE-2                                         XEN  246
      SR=0.0                                                            XEN  247
      M1=M+1                                                            XEN  248
      M2=M+2                                                            XEN  249
      DO 380 I=1,KTR                                                    XEN  250
      ALFX=E(I,M1)                                                      XEN  251
      BETA=E(I,M2)                                                      XEN  252
      XM=M                                                              XEN  253
      QR=(XLOGDA-XM) * (BETA-ALFX) + ALFX                               XEN  254
      IF (KT1.NE.0) PRINT 370,XM,ALFX,BETA                              XEN  255
 370  FORMAT(1H ,3F20.8)                                                XEN  256
      Q(I)=QR                                                           XEN  257
      SR=SR+QR                                                          XEN  258
 380  S(I)=SR                                                           XEN  259
      IF (KT1.NE.0) PRINT 390,(Q(I),I=1,KTR),(S(I),I=1,KTR)             XEN  260
 390  FORMAT(1H ,12F9.4)                                                XEN  261
C                                                                       XEN  262
C     PART 4,DEFINE INSTANT TA OF EACH POINT IN SEQUENCE A              XEN  263
C                                                                       XEN  264
      IF (KTEST2.NE.0) TAV2=TIMEF(1)                                    XEN  265
      N=1                                                               XEN  266
      T=0.0                                                             XEN  267
      TA=0.0                                                            XEN  268
      GO TO 410                                                         XEN  269
 400  N=N+1                                                             XEN  270
      X=RANF(-1)                                                        XEN  271      
      T=-LOGF(X)/DA                                                     XEN  272      
      TA=TA+T                                                           XEN  273      
 410  IF (KT1.NE.0) PRINT 420,N,X,T,TA                                  XEN  274
 420  FORMAT(//,I8,3E20.8)                                              XEN  275
C                                                                       XEN  276
C     PART 5,DEFINE CLASS AND INSTRUMENT NUMBER TO EACH POINT OF A      XEN  277
C                                                                       XEN  278
      X1=RANF(-1)                                                       XEN  279
      DO 430 I=1,KTR                                                    XEN  280
 430  IF (X1.LE.S(I)) GO TO 440                                         XEN  281
      I=KTR                                                             XEN  282
 440  KTS=NT(I)                                                         XEN  283
      KR=I                                                              XEN  284
      X2=RANF(-1)                                                       XEN  285
      DO 450 J=1,KTS                                                    XEN  286
      SPIEN=SPN(KR,J)                                                   XEN  287
      INSTRM=J                                                          XEN  288
 450  IF (X2.LE.SPIEN) GO TO 460                                        XEN  289
      INSTRM=KTS                                                        XEN  290
 460  PIEN=PN(KR,INSTRM)                                                XEN  291
      IF (KT1.NE.0) PRINT 470,X1,S(KR),KR,X2,SPIEN,INSTRM               XEN  292
 470  FORMAT( 1H ,2E20.8,I6,2E20.8,I6 )                                 XEN  293
C                                                                       XEN  294
C     PART 6,DEFINE PITCH HN FOR EACH POINT OF SEQUENCE A               XEN  295
C                                                                       XEN  296
      IF (KR.GT.1) GO TO 480                                            XEN  297
      IF (INSTRM.GE.KR1) GO TO 490                                      XEN  298
      HX=0.0                                                            XEN  299
      GO TO 560                                                         XEN  300
 480  IF (KR.LT.7) GO TO 490                                            XEN  301
      HSUP=HBMAX(KR,INSTRM)                                             XEN  302
      HINF=HBMIN(KR,INSTRM)                                             XEN  303
      GO TO 500                                                         XEN  304
 490  HSUP=HAMAX(KR,INSTRM)                                             XEN  305
      HINF=HAMIN(KR,INSTRM)                                             XEN  306
 500  HM=HSUP-HINF                                                      XEN  307
      HPR=H(KR,INSTRM)                                                  XEN  308
      K=0                                                               XEN  309
      IF (HPR.LE.0.0) GO TO 520                                         XEN  310
 510  X=RANF(-1)                                                        XEN  311
      IF (N.GT.1) GO TO 530                                             XEN  312
 520  HX=HINF+HM*X RANF(-1)                                             XEN  313
      GO TO 560                                                         XEN  314
 530  IF(RANF(-1).GE.0.5) GO TO 540                                     XEN  315
      HX=HPR+HM * ( 1.0-SQRTF(X))                                       XEN  316
      GO TO 550                                                         XEN  317
 540  HX=HPR-HM * (1.0-SQRTF(X))                                        XEN  318
 550  IF((HX.GE.HINF).AND.(HX.LE.HSUP)) GO TO 560                       XEN  319
      IF (K.GE.KT2) GO TO 520                                           XEN  320
      K=K+1                                                             XEN  321
      GO TO 510                                                         XEN  322
 560  H(KR,INSTRM)=HX                                                   XEN  323
      IF (KT1.NE.0) PRINT 570,K,X,HX                                    XEN  324
 570  FORMAT(1H ,I6,2E20.8)                                             XEN  325
C                                                                       XEN  326
C     PART 7,DEFINE SPEED VIGL TO EACH POINT OF A                       XEN  327
C                                                                       XEN  328
      IF (KR.EQ.5) GO TO 580                                            XEN  329
      VIGL(1)=0.0                                                       XEN  330
      VIGL(2)=0.0                                                       XEN  331
      VIGL(3)=0.0                                                       XEN  332
      X1=0.0                                                            XEN  333
      X2=0.0                                                            XEN  334
      XLAMBDA=0.0                                                       XEN  335
      GO TO 740                                                         XEN  336
 580  KX=1                                                              XEN  337
 590  X1=RANF(-1)                                                       XEN  338
      IF (X1-0.9997) 600,650,680                                        XEN  339
 600  I=128                                                             XEN  340
      DO 630 IX=1,7                                                     XEN  341
      IF(TETA(I)-X1) 610,640,620                                        XEN  342
 610  I=I+MODI(IX)                                                      XEN  343
      GO TO 630                                                         XEN  344
 620  I=I-MODI(IX)                                                      XEN  345
 630  CONTINUE                                                          XEN  346
      IF(TETA(I)-X1) 670,640,660                                        XEN  347
 640  XLAMBDA=FLOATF(I-1)/100.0                                         XEN  348
      GO TO (720,760), KX                                               XEN  349
 650  XLAMBDA=2.55                                                      XEN  350
      GO TO (720,760),KX                                                XEN  351
 660  I=I-1                                                             XEN  352
 670  TX1=TETA(I)                                                       XEN  353
      XLAMBDA=(FLOATF(I-1)+(X1-TX1)/(TETA(I+1)-TX1))/100.0              XEN  354
      GO TO ( 720,760 ), KX                                             XEN  355
 680  DO 690 I=2,7                                                      XEN  356
      TX1=Z2(I)                                                         XEN  357
      IF(X1-TX1) 700,710,690                                            XEN  358
 690  CONTINUE                                                          XEN  359
      I=8                                                               XEN  360
      TX1=1.0                                                           XEN  361
 700  TX2=Z1(I)                                                         XEN  362
      XLAMBDA=TX2-((TX1-X1)/(TX1-Z2(I-1)))*(TX2-Z1(I-1))                XEN  363
      GO TO ( 720,760 ), KX                                             XEN  364
 710  XLAMBDA=Z1(I)                                                     XEN  365
      GO TO( 720,760 ), KX                                              XEN  366
 720  ALFA(1)=A10+XALOG                                                 XEN  367
      ALFA(3)=A30-XALOG                                                 XEN  368
      X2=RANF(-1)                                                       XEN  369
      ALFA(2)=A17+A35*X2                                                XEN  370
      DO 730 I=1,3                                                      XEN  371
      VIGL(I)=INTF(ALFA(I)*XLAMBDA+0.5)                                 XEN  372
      IF (VIGL(I).LT.0.0) VIGL(I)=-VIGL(I)                              XEN  373
      IF (VIGL(I).GT.VITLIM) VIGL(I)=VITLIM                             XEN  374
 730  IF (RANF(-1).LT.0.5) VIGL(I)=-VIGL(I)                             XEN  375
 740  IF(KT1.NE.0) PRINT 750,X1,X2,XLAMBDA,VIGL                         XEN  376
 750  FORMAT(1H ,6E19.8)                                                XEN  377
C                                                                       XEN  378
C     PART 8,DEFINE DURATION FOR EACH POINT OF A                        XEN  379
C                                                                       XEN  380
      IF ((KR.EQ.7).OR.(KR.EQ.8)) GO TO 780                             XEN  381
      ZMAX=AMAX(KR)/(V3*PIEN)                                           XEN  382
      G=GN(KR,INSTRM)                                                   XEN  383
      RO=G/LOGF(ZMAX)                                                   XEN  384
      QPNDA=1.0/(Q(KR)*PIEN*DA)                                         XEN  385
      GE=ABSF(RO*LOGF(QPNDA))                                           XEN  386
      XMU=GE/2.0                                                        XEN  387
      SIGMA=GE/4.0                                                      XEN  388
      KX=2                                                              XEN  389
      GO TO 590                                                         XEN  390
 760  TAU=SIGMA*XLAMBDA*1.4142                                          XEN  391
      X2=RANF(-1)                                                       XEN  392
      IF (X2.GE.0.5) GO TO 770                                          XEN  393
      XDUR=XMU+TAU                                                      XEN  394
      GO TO 790                                                         XEN  395
 770  XDUR=XMU-TAU                                                      XEN  396
      IF (XDUR.GE.0.0) GO TO 790                                        XEN  397
 780  XDUR=0.0                                                          XEN  398
 790  IF(KT1.NE.0)PRINT 800,ZMAX,XMU,SIGMA,X1,XLAMBDA,X2,XDUR           XEN  399
 800  FORMAT(1H ,5E15.8,E11.4,E15.8)                                    XEN  400
C                                                                       XEN  401
C     PART 9,DEFINE INTENSITY FORM TO EACH POINT OF A                   XEN  402
C                                                                       XEN  403
      IFORM=XINTF(RANF(-1)*BF+0.5)                                      XEN  404
      IF (KT1.EQ.0) GO TO 840                                           XEN  405
      IF (NLINE.LT.KNL) GO TO 810                                       XEN  406
      IF (NLINE.EQ.KNL) GO TO 820                                       XEN  407
      NLINE=1                                                           XEN  408
      GO TO 900                                                         XEN  409
 810  NLINE=NLINE+1                                                     XEN  410
      GO TO 900                                                         XEN  411
 820  PRINT 830                                                         XEN  412
 830  FORMAT(1H1)                                                       XEN  413
      NLINE=0                                                           XEN  414
      GO TO 900                                                         XEN  415
 840  IF (NLINE.GE.KNL) GO TO 850                                       XEN  416
      NLINE=NLINE+1                                                     XEN  417
      GO TO 880                                                         XEN  418
 850  PRINT 860,JW,A,NA,(Q(I),I=1,KTR)                                  XEN  419
 860  FORMAT('1  JW=',I3,4X,'A=',F8.2,4X,'NA=',I6,4X,'Q(I)=',12(F4.2,'/'XEN  420
     1),//)                                                             XEN  421
      PRINT 870                                                         XEN  422
 870  FORMAT(6X,'N',8X,'START',5X,'CLASS',4X,'INSTRM',4X,'PITCH',6X,    XEN  423
     1'GLISS1',4X,'GLISS2',4X,'GLISS3',8X,'DURATION',5X,'DYNAM')        XEN  424
      NLINE=1                                                           XEN  425
 880  PRINT 890,N,TA,KR,INSTRM,HX,(VIGL(I),I=1,3),XDUR,IFORM            XEN  426
 890  FORMAT(1H ,I7,F12.2,I9,I8,F11.1,F13.1,2F10.1,F14.2,I11)           XEN  427
C                                                                       XEN  428
C     PART 10,REPEAT SAME DEFINITIONS FOR ALL POINTS OF A               XEN  429
C                                                                       XEN  430
 900  IF (N.LT.NA) GO TO 400                                            XEN  431
C                                                                       XEN  432
C     PART 11, REPEAT SEQUENCES A                                       XEN  433
C                                                                       XEN  434
      IF (KTEST2.EQ.0) GO TO 910                                        XEN  435
      TAP2=TIMEF(1)-TAV2                                                XEN  436
      TAP2=TAP2/FLOATF(NA)                                              XEN  437
      PRINT 750,TAP2                                                    XEN  438
C                                                                       XEN  439
 910  IF (JW.GE.KW) GO TO 930                                           XEN  440
 920  JW=JW+1                                                           XEN  441
      IF (GTNS.GT.SINA) GO TO 220                                       XEN  442
 930  IF (KTEST1.EQ.0) CALL EXIT                                        XEN  443
 940  TAP1=TIMEF(-1)-TAV1                                               XEN  444
      TAP1=TAP1/FLOATF(KW)                                              XEN  445
      PRINT 750,TAP1                                                    XEN  446
C                                                                       XEN  447
      END                                                               XEN  448
