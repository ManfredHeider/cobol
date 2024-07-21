      ***************************************************************** 00010000
      *    COPY-MEMBER: DROP0017  SPNP-STATISTIKSATZ (DROM6255)       * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   2    27.02.95 10:27:45
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: PROEMPELER                                       * 00060000
      *    DATUM:    19.01.1995                                       * 00070000
      *****************************************************************
      ****************************************************************  00091000
      * AENDERUNGEN:                                                 *  00092000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00093000
      * -------------------------------------------------------------*  00094000
      * AFOP0055!19.01.95! 01 ! NEU                       !WIECHERS  *  00095002
      * AFOP0055!24.02.95! 02 ! AENDERUNG: ZUSAETZLICHE   !WIECHERS  *  00096001
      *         !        !    !            FELDER         !          *  00099200
      * --------!20.01.99! -- ! NEUES DATENFELD           !WIECHERS  *  00099300
      *         !        ! -- ! EG-BEW-EING-GESCH-D       !          *  00099300
      * AOP00189!18.10.99! -- ! NEUES DATENFELD           !WIECHERS  *  00099300
      *         !        ! -- ! DIREKTION                 !          *  00099300
      *                                                              *  00099400
      * ENDE-LOGBUCH                                                 *  00099500
      ****************************************************************  00091000
       01  DROM6255.
           03 SATZ-KZ                   PICTURE X(4).
           03 GESKZ                     PICTURE XX.
           03 GESBEZ-LANG               PICTURE X(40).
           03 LKZ                       PICTURE X(4).
           03 PRODUKTSCHL               PICTURE X(6).
           03 PRODUKTBEZ-LANG           PICTURE X(40).
           03 DATEN-SPNP.
              05 BEW-BETR-ZUG           PICTURE S9(11)V99
000100                                                 COMPUTATIONAL-3.
000110        05 BEW-BETR-FOLG          PICTURE S9(11)V99
000120                                                 COMPUTATIONAL-3.
000130        05 BEW-BETR-ABG           PICTURE S9(11)V99
000140                                                 COMPUTATIONAL-3.
000150        05 BEW-BETR-NETTO         PICTURE S9(11)V99
000160                                                 COMPUTATIONAL-3.
000170        05 PROZ-NETTO             PICTURE S999V99
000180                                                 COMPUTATIONAL-3.
000190        05 PROVEINH               PICTURE S9(9)V9
000200                                                 COMPUTATIONAL-3.
000210        05 PROVEINH-ZUG           PICTURE S9(9)V9
000220                                                 COMPUTATIONAL-3.
000230        05 PROVEINH-AZUG          PICTURE S9(9)V9
000240                                                 COMPUTATIONAL-3.
000250        05 PROVEINH-ABG           PICTURE S9(9)V9
000260                                                 COMPUTATIONAL-3.
000270        05 PROVEINH-AABG          PICTURE S9(9)V9
000280                                                 COMPUTATIONAL-3.
000290        05 PROZ-EINH              PICTURE S999V99
000300                                                 COMPUTATIONAL-3.
000310        05 ANZAHL                 PICTURE S9(13) COMPUTATIONAL-3.
000320        05 EQF                    PICTURE S9(6)V99
000330                                                 COMPUTATIONAL-3.
000340     03 DATEN-EING-GESCH-FILLER REDEFINES DATEN-SPNP.
000350        05 DATEN-EING-GESCH.
000360           07 EG-BEW-BETR-ZUG     PICTURE S9(11)V99
000370                                                 COMPUTATIONAL-3.
000380           07 EG-BEW-BETR-ABG     PICTURE S9(11)V99
000390                                                 COMPUTATIONAL-3.
000400           07 EG-BEW-BETR-AUFABG  PICTURE S9(11)V99
000410                                                 COMPUTATIONAL-3.
000420           07 EG-BEW-BETR-AUFZUG  PICTURE S9(11)V99
000430                                                 COMPUTATIONAL-3.
000440           07 EG-BEW-BETR-NETTO   PICTURE S9(11)V99
000450                                                 COMPUTATIONAL-3.
000460           07 EG-PROVEINH         PICTURE S9(9)V9
000470                                                 COMPUTATIONAL-3.
000480           07 EG-EQF              PICTURE S9(6)V99
000490                                                 COMPUTATIONAL-3.
000500           07 EG-BEW-BETR-DYN     PICTURE S9(11)V99
000510                                                 COMPUTATIONAL-3.
000500           07 EG-BEW-EING-GESCH-D PICTURE S9(09)V9
000510                                                 COMPUTATIONAL-3.
000520        05 FILLER                 PICTURE X(17).
991018     03 DIR                       PICTURE X(03).
