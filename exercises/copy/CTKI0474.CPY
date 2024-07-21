      ******************************************************************
      * DCLGEN TABLE(T13KIS.FREMDVERSICHERER)                          *
      *        LIBRARY(T130005.OZPGEN.SRCDCL(CTKI0474))                *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(FREMDVERSICHERER-GESAMT)                      *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE FREMDVERSICHERER TABLE
CCC004*    ( FRVVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      FRVVETLFDNR                    SMALLINT NOT NULL,
CCC004*      FRVRSKGRUPPEID                 SMALLINT NOT NULL,
CCC004*      FRVTHEMGSCHL_KTY               SMALLINT NOT NULL,
CCC004*      FRVTHEMGSCHL_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      FRVUNIVLFDNR                   SMALLINT NOT NULL,
CCC004*      FRVEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      FRVDIFFGEF_KTY                 SMALLINT NOT NULL,
CCC004*      FRVDIFFGEF_KNR                 DECIMAL(15, 0) NOT NULL,
CCC004*      FRVDIFFGES_KTY                 SMALLINT NOT NULL,
CCC004*      FRVDIFFGES_KNR                 DECIMAL(15, 0) NOT NULL,
CCC004*      FRVVERSNR                      CHAR(40),
CCC004*      FRVUMSTELLTERMIN               DATE,
CCC004*      FRVKUENDIGUNGKZ                CHAR(2),
CCC004*      FRVRSKMERKMAL                  DECIMAL(9, 0),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE FREMDVERSICHERER            *
      ******************************************************************
       01  FREMDVERSICHERER-GESAMT.
        05 NI-FREMDVERSICHERER-TAB.
           10 NI-FREMDVERSICHERER-ELE PIC S9(04) COMP OCCURS 16 TIMES.
        05 NI-FREMDVERSICHERER REDEFINES NI-FREMDVERSICHERER-TAB.
           10 NI-FRVVERTRAGSID      PIC S9(4) COMP.
           10 NI-FRVVETLFDNR        PIC S9(4) COMP.
           10 NI-FRVRSKGRUPPEID     PIC S9(4) COMP.
           10 NI-FRVTHEMGSCHL-KTY   PIC S9(4) COMP.
           10 NI-FRVTHEMGSCHL-KNR   PIC S9(4) COMP.
           10 NI-FRVUNIVLFDNR       PIC S9(4) COMP.
           10 NI-FRVEINZVTGHOSTID   PIC S9(4) COMP.
           10 NI-FRVDIFFGEF-KTY     PIC S9(4) COMP.
           10 NI-FRVDIFFGEF-KNR     PIC S9(4) COMP.
           10 NI-FRVDIFFGES-KTY     PIC S9(4) COMP.
           10 NI-FRVDIFFGES-KNR     PIC S9(4) COMP.
           10 NI-FRVVERSNR          PIC S9(4) COMP.
           10 NI-FRVUMSTELLTERMIN   PIC S9(4) COMP.
           10 NI-FRVKUENDIGUNGKZ    PIC S9(4) COMP.
           10 NI-FRVRSKMERKMAL      PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-FREMDVERSICHERER.
           10 FRVVERTRAGSID        PIC X(13).
           10 FRVVETLFDNR          PIC S9(4) USAGE COMP.
           10 FRVRSKGRUPPEID       PIC S9(4) USAGE COMP.
           10 FRVTHEMGSCHL-KTY     PIC S9(4) USAGE COMP.
           10 FRVTHEMGSCHL-KNR     PIC S9(15)V USAGE COMP-3.
           10 FRVUNIVLFDNR         PIC S9(4) USAGE COMP.
           10 FRVEINZVTGHOSTID     PIC X(15).
           10 FRVDIFFGEF-KTY       PIC S9(4) USAGE COMP.
           10 FRVDIFFGEF-KNR       PIC S9(15)V USAGE COMP-3.
           10 FRVDIFFGES-KTY       PIC S9(4) USAGE COMP.
           10 FRVDIFFGES-KNR       PIC S9(15)V USAGE COMP-3.
           10 FRVVERSNR            PIC X(40).
           10 FRVUMSTELLTERMIN     PIC X(10).
           10 FRVKUENDIGUNGKZ      PIC X(2).
           10 FRVRSKMERKMAL        PIC S9(9)V USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************

