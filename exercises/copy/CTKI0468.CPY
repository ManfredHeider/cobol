CCC005*    EXEC SQL DECLARE G10KSDSSUHSA400 TABLE
CCC004*    ( G10VVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G10VMANDANT                    INTEGER NOT NULL,
CCC004*      G10VABSCHLVBNR                 INTEGER NOT NULL,
CCC004*      G10VHERKUNFT                   CHAR(3) NOT NULL,
CCC004*      G10VAENDKZ                     CHAR(2) NOT NULL,
CCC004*      G10VPIN                        CHAR(9) NOT NULL,
CCC004*      G10VPIN_PRUEF                  CHAR(2) NOT NULL,
CCC004*      G10VQUITTNR                    CHAR(15) NOT NULL,
CCC004*      G10VERSTDATUM                  DATE NOT NULL,
CCC004*      G10VERSTZEIT                   INTEGER NOT NULL,
CCC004*      G10VANTNR                      CHAR(18) NOT NULL,
CCC004*      G10V1MERKM                     CHAR(20) NOT NULL,
CCC004*      G10V1MNR                       CHAR(10) NOT NULL,
CCC004*      G10V2MERKM                     CHAR(20) NOT NULL,
CCC004*      G10V2MNR                       CHAR(10) NOT NULL,
CCC004*      G10VCPBRDNR                    CHAR(50) NOT NULL,
CCC004*      G10VSPARTE                     CHAR(9) NOT NULL,
CCC004*      G10VVART                       INTEGER NOT NULL,
CCC004*      G10VBEGINN                     DATE NOT NULL,
CCC004*      G10VABLAUF                     DATE NOT NULL,
CCC004*      G10VSTATUS                     CHAR(9) NOT NULL,
CCC004*      G10VVERSRISIKO                 CHAR(23) NOT NULL,
CCC004*      G10VAENDDAT                    DATE NOT NULL,
CCC004*      G10VAENDGRD                    CHAR(50) NOT NULL,
CCC004*      G10VAFBPROZ                    DECIMAL(5, 3) NOT NULL,
CCC004*      G10VBUENDKZ                    CHAR(1) NOT NULL,
CCC004*      G10VINFO                       CHAR(80) NOT NULL,
CCC004*      G10VSUMME                      DECIMAL(10, 0) NOT NULL,
CCC004*      G10VURWAEHR                    CHAR(9) NOT NULL,
CCC004*      G10VINKFORM                    CHAR(30) NOT NULL,
CCC004*      G10VZAHLWEISE                  CHAR(9) NOT NULL,
CCC004*      G10VNINKBTR                    DECIMAL(12, 2) NOT NULL,
CCC004*      G10VINKJGBTR                   DECIMAL(12, 2) NOT NULL,
CCC004*      G10VFLGDAT                     DATE NOT NULL,
CCC004*      G10VINKJGNBTR                  DECIMAL(10, 2) NOT NULL,
CCC004*      G10VINKMHNSTF                  CHAR(25) NOT NULL,
CCC004*      G10VINKMHNDAT                  DATE NOT NULL,
CCC004*      G10VINKRBTR                    DECIMAL(9, 2) NOT NULL,
CCC004*      G10VINKLFZRAB                  SMALLINT NOT NULL,
CCC004*      G10VINKRATZU                   SMALLINT NOT NULL,
CCC004*      G10VZEICHNUNG                  CHAR(25) NOT NULL,
CCC004*      G10VINKFBBTR                   DECIMAL(10, 2) NOT NULL,
CCC004*      G10VINKFBSUMME                 INTEGER NOT NULL,
CCC004*      G10VINKELBTRB                  DECIMAL(10, 2) NOT NULL,
CCC004*      G10VINKELBTRN                  DECIMAL(10, 2) NOT NULL,
CCC004*      G10VINKLSTSYN                  DATE NOT NULL,
CCC004*      G10VINKLSTAGT                  DATE NOT NULL,
CCC004*      G10VINKLSTBEWG                 CHAR(61) NOT NULL,
CCC004*      G10VINKKDBON                   CHAR(26) NOT NULL,
CCC004*      G10VINKOSTRAB                  SMALLINT NOT NULL,
CCC004*      G10VINKBSA                     CHAR(6) NOT NULL,
CCC004*      G10VGBSALTXT                   CHAR(50) NOT NULL,
CCC004*      G10VCGESELL                    CHAR(50) NOT NULL,
CCC004*      G10VNVDNR                      INTEGER NOT NULL,
CCC004*      G10VVDNAME                     CHAR(20) NOT NULL,
CCC004*      G10VCVORNR                     CHAR(17) NOT NULL,
CCC004*      ZUGANG                         TIMESTAMP(12) NOT NULL,
CCC004*      VON                            TIMESTAMP(12) NOT NULL,
CCC004*      BIS                            TIMESTAMP(12) NOT NULL,
CCC004*      G10VAKTIONERG                  CHAR(3),
CCC004*      G10VBESTMANAGKZ                CHAR(1),
CCC004*      G10VBESCHRBMKZ                 CHAR(40),
CCC004*      G10VCOCDATUM                   DATE,
CCC004*      G10VCOCKZ                      CHAR(1),
CCC004*      G10VPRKOMBIBEZ                 CHAR(50),
CCC004*      G10VKUNDENIDSMH                CHAR(10),
CCC004*      G10VFLEXNVSPNL                 DECIMAL(11, 2),
CCC004*      G10VFLEXNVSPKZ                 CHAR(1),
CCC004*      G10VFLEXNVGVNL                 DECIMAL(11, 2),
CCC004*      G10VFLEXNVGVKZ                 CHAR(1),
CCC004*      G10VVSPKZ                      CHAR(3)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G10KSDSSUHSA400             *
      ******************************************************************
       01  G10KSDSSUHSA400-GESAMT.
        05 NI-G10KSDSSUHSA400-TAB.
           10 NI-G10KSDSSUHSA400-ELE PIC S9(04) COMP OCCURS 71 TIMES.
        05 NI-G10KSDSSUHSA400 REDEFINES NI-G10KSDSSUHSA400-TAB.
           10 NI-G10VVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G10VMANDANT        PIC S9(4) COMP.
           10 NI-G10VABSCHLVBNR     PIC S9(4) COMP.
           10 NI-G10VHERKUNFT       PIC S9(4) COMP.
           10 NI-G10VAENDKZ         PIC S9(4) COMP.
           10 NI-G10VPIN            PIC S9(4) COMP.
           10 NI-G10VPIN-PRUEF      PIC S9(4) COMP.
           10 NI-G10VQUITTNR        PIC S9(4) COMP.
           10 NI-G10VERSTDATUM      PIC S9(4) COMP.
           10 NI-G10VERSTZEIT       PIC S9(4) COMP.
           10 NI-G10VANTNR          PIC S9(4) COMP.
           10 NI-G10V1MERKM         PIC S9(4) COMP.
           10 NI-G10V1MNR           PIC S9(4) COMP.
           10 NI-G10V2MERKM         PIC S9(4) COMP.
           10 NI-G10V2MNR           PIC S9(4) COMP.
           10 NI-G10VCPBRDNR        PIC S9(4) COMP.
           10 NI-G10VSPARTE         PIC S9(4) COMP.
           10 NI-G10VVART           PIC S9(4) COMP.
           10 NI-G10VBEGINN         PIC S9(4) COMP.
           10 NI-G10VABLAUF         PIC S9(4) COMP.
           10 NI-G10VSTATUS         PIC S9(4) COMP.
           10 NI-G10VVERSRISIKO     PIC S9(4) COMP.
           10 NI-G10VAENDDAT        PIC S9(4) COMP.
           10 NI-G10VAENDGRD        PIC S9(4) COMP.
           10 NI-G10VAFBPROZ        PIC S9(4) COMP.
           10 NI-G10VBUENDKZ        PIC S9(4) COMP.
           10 NI-G10VINFO           PIC S9(4) COMP.
           10 NI-G10VSUMME          PIC S9(4) COMP.
           10 NI-G10VURWAEHR        PIC S9(4) COMP.
           10 NI-G10VINKFORM        PIC S9(4) COMP.
           10 NI-G10VZAHLWEISE      PIC S9(4) COMP.
           10 NI-G10VNINKBTR        PIC S9(4) COMP.
           10 NI-G10VINKJGBTR       PIC S9(4) COMP.
           10 NI-G10VFLGDAT         PIC S9(4) COMP.
           10 NI-G10VINKJGNBTR      PIC S9(4) COMP.
           10 NI-G10VINKMHNSTF      PIC S9(4) COMP.
           10 NI-G10VINKMHNDAT      PIC S9(4) COMP.
           10 NI-G10VINKRBTR        PIC S9(4) COMP.
           10 NI-G10VINKLFZRAB      PIC S9(4) COMP.
           10 NI-G10VINKRATZU       PIC S9(4) COMP.
           10 NI-G10VZEICHNUNG      PIC S9(4) COMP.
           10 NI-G10VINKFBBTR       PIC S9(4) COMP.
           10 NI-G10VINKFBSUMME     PIC S9(4) COMP.
           10 NI-G10VINKELBTRB      PIC S9(4) COMP.
           10 NI-G10VINKELBTRN      PIC S9(4) COMP.
           10 NI-G10VINKLSTSYN      PIC S9(4) COMP.
           10 NI-G10VINKLSTAGT      PIC S9(4) COMP.
           10 NI-G10VINKLSTBEWG     PIC S9(4) COMP.
           10 NI-G10VINKKDBON       PIC S9(4) COMP.
           10 NI-G10VINKOSTRAB      PIC S9(4) COMP.
           10 NI-G10VINKBSA         PIC S9(4) COMP.
           10 NI-G10VGBSALTXT       PIC S9(4) COMP.
           10 NI-G10VCGESELL        PIC S9(4) COMP.
           10 NI-G10VNVDNR          PIC S9(4) COMP.
           10 NI-G10VVDNAME         PIC S9(4) COMP.
           10 NI-G10VCVORNR         PIC S9(4) COMP.
           10 NI-ZUGANG             PIC S9(4) COMP.
           10 NI-VON                PIC S9(4) COMP.
           10 NI-BIS                PIC S9(4) COMP.
           10 NI-G10VAKTIONERG      PIC S9(4) COMP.
           10 NI-G10VBESTMANAGKZ    PIC S9(4) COMP.
           10 NI-G10VBESCHRBMKZ     PIC S9(4) COMP.
           10 NI-G10VCOCDATUM       PIC S9(4) COMP.
           10 NI-G10VCOCKZ          PIC S9(4) COMP.
           10 NI-G10VPRKOMBIBEZ     PIC S9(4) COMP.
           10 NI-G10VKUNDENIDSMH    PIC S9(4) COMP.
           10 NI-G10VFLEXNVSPNL     PIC S9(4) COMP.
           10 NI-G10VFLEXNVSPKZ     PIC S9(4) COMP.
           10 NI-G10VFLEXNVGVNL     PIC S9(4) COMP.
           10 NI-G10VFLEXNVGVKZ     PIC S9(4) COMP.
           10 NI-G10VVSPKZ          PIC S9(4) COMP.
        05 DCL-G10KSDSSUHSA400.
           10 G10VVERTRAGSNR       PIC X(15).
           10 G10VMANDANT          PIC S9(9) USAGE COMP.
           10 G10VABSCHLVBNR       PIC S9(9) USAGE COMP.
           10 G10VHERKUNFT         PIC X(3).
           10 G10VAENDKZ           PIC X(2).
           10 G10VPIN              PIC X(9).
           10 G10VPIN-PRUEF        PIC X(2).
           10 G10VQUITTNR          PIC X(15).
           10 G10VERSTDATUM        PIC X(10).
           10 G10VERSTZEIT         PIC S9(9) USAGE COMP.
           10 G10VANTNR            PIC X(18).
           10 G10V1MERKM           PIC X(20).
           10 G10V1MNR             PIC X(10).
           10 G10V2MERKM           PIC X(20).
           10 G10V2MNR             PIC X(10).
           10 G10VCPBRDNR          PIC X(50).
           10 G10VSPARTE           PIC X(9).
           10 G10VVART             PIC S9(9) USAGE COMP.
           10 G10VBEGINN           PIC X(10).
           10 G10VABLAUF           PIC X(10).
           10 G10VSTATUS           PIC X(9).
           10 G10VVERSRISIKO       PIC X(23).
           10 G10VAENDDAT          PIC X(10).
           10 G10VAENDGRD          PIC X(50).
           10 G10VAFBPROZ          PIC S9(2)V9(3) USAGE COMP-3.
           10 G10VBUENDKZ          PIC X(1).
           10 G10VINFO             PIC X(80).
           10 G10VSUMME            PIC S9(10)V USAGE COMP-3.
           10 G10VURWAEHR          PIC X(9).
           10 G10VINKFORM          PIC X(30).
           10 G10VZAHLWEISE        PIC X(9).
           10 G10VNINKBTR          PIC S9(10)V9(2) USAGE COMP-3.
           10 G10VINKJGBTR         PIC S9(10)V9(2) USAGE COMP-3.
           10 G10VFLGDAT           PIC X(10).
           10 G10VINKJGNBTR        PIC S9(8)V9(2) USAGE COMP-3.
           10 G10VINKMHNSTF        PIC X(25).
           10 G10VINKMHNDAT        PIC X(10).
           10 G10VINKRBTR          PIC S9(7)V9(2) USAGE COMP-3.
           10 G10VINKLFZRAB        PIC S9(4) USAGE COMP.
           10 G10VINKRATZU         PIC S9(4) USAGE COMP.
           10 G10VZEICHNUNG        PIC X(25).
           10 G10VINKFBBTR         PIC S9(8)V9(2) USAGE COMP-3.
           10 G10VINKFBSUMME       PIC S9(9) USAGE COMP.
           10 G10VINKELBTRB        PIC S9(8)V9(2) USAGE COMP-3.
           10 G10VINKELBTRN        PIC S9(8)V9(2) USAGE COMP-3.
           10 G10VINKLSTSYN        PIC X(10).
           10 G10VINKLSTAGT        PIC X(10).
           10 G10VINKLSTBEWG       PIC X(61).
           10 G10VINKKDBON         PIC X(26).
           10 G10VINKOSTRAB        PIC S9(4) USAGE COMP.
           10 G10VINKBSA           PIC X(6).
           10 G10VGBSALTXT         PIC X(50).
           10 G10VCGESELL          PIC X(50).
           10 G10VNVDNR            PIC S9(9) USAGE COMP.
           10 G10VVDNAME           PIC X(20).
           10 G10VCVORNR           PIC X(17).
           10 ZUGANG               PIC X(32).
           10 VON                  PIC X(32).
           10 BIS                  PIC X(32).
           10 G10VAKTIONERG        PIC X(3).
           10 G10VBESTMANAGKZ      PIC X(1).
           10 G10VBESCHRBMKZ       PIC X(40).
           10 G10VCOCDATUM         PIC X(10).
           10 G10VCOCKZ            PIC X(1).
           10 G10VPRKOMBIBEZ       PIC X(50).
           10 G10VKUNDENIDSMH      PIC X(10).
           10 G10VFLEXNVSPNL       PIC S9(9)V9(2) USAGE COMP-3.
           10 G10VFLEXNVSPKZ       PIC X(1).
           10 G10VFLEXNVGVNL       PIC S9(9)V9(2) USAGE COMP-3.
           10 G10VFLEXNVGVKZ       PIC X(1).
           10 G10VVSPKZ            PIC X(3).
      ************************** ENDE-DCLGEN *************************

