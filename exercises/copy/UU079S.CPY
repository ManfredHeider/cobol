      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU079S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU079S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:53:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE VOLKSFÜRSORGE                *
      *    --------------------------------------------                *
      *                                                                *
      *    LAENGE = 256 BYTE         ERSTELLT AM 04.10.90.             *
      *                                          KUHN                  *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU079S.
           10  VOF-SA                  PIC 9(4).
           10  VOF-SATZ-0001.
               20  VOF-VUNR-01         PIC X(5).
               20  VOF-ABSENDER        PIC X(30).
               20  VOF-EMPFAENGER      PIC X(30).
               20  VOF-ERSTDAT-VOM     PIC 9(8).
               20  VOF-ERSTDAT-BIS     PIC 9(8).
               20  VOF-AGENTUR-01      PIC X(10).
               20  VOF-FREI-0001       PIC X(161).
CCC009*    SKIP1
           10  VOF-SATZ-9999   REDEFINES   VOF-SATZ-0001.
               20  VOF-SATZANZ         PIC 9(10).
               20  VOF-AGENTUR-99      PIC X(10).
               20  VOF-GES-BTR         PIC 9(13)V99.
               20  VOF-GES-BTR-BR      PIC 9(13)V99.
               20  VOF-GES-PROV        PIC 9(13)V99.
               20  VOF-GES-VSL         PIC 9(13)V99.
               20  VOF-GES-SBAK        PIC 9(13)V99.
               20  VOF-FREI-9999       PIC X(157).
CCC009*    SKIP1
           10  VOF-SATZ-0100   REDEFINES   VOF-SATZ-0001.
               20  VOF-VUNR            PIC X(5).
               20  VOF-BKZ             PIC X.
               20  VOF-SPARTE          PIC 9(3).
               20  VOF-VSNR            PIC X(17).
               20  VOF-FOLGENR         PIC 99.
               20  VOF-AGENTUR         PIC X(10).
               20  VOF-ADRESS-TEIL.
                  25  VOF-ANREDE       PIC X.
                  25  VOF-NAME-1       PIC X(30).
                  25  VOF-NAME-2       PIC X(30).
                  25  VOF-NAME-3       PIC X(30).
                  25  VOF-TITEL        PIC X(20).
                  25  VOF-AUSL         PIC X(3).
                  25  VOF-PLZ          PIC X(6).
                  25  VOF-ORT          PIC X(25).
                  25  VOF-STR          PIC X(30).
                  25  VOF-PSTF         PIC X(8).
                  25  VOF-GEBD         PIC 9(8).
                  25  VOF-STAK         PIC X(3).
                  25  VOF-ADRKZ        PIC XX.
                  25  VOF-AKTKZ        PIC X(12).
                  25  VOF-ZGKZ         PIC XX.
                  25  VOF-FREI-0100    PIC X(4).
CCC009*    SKIP1
               20  VOF-VERTRAG-ALLGEM  REDEFINES  VOF-ADRESS-TEIL.
                  25  VOF-INKASSO      PIC X.
                  25  VOF-VBEG         PIC 9(8).
                  25  VOF-VABL         PIC 9(8).
                  25  VOF-HFAEL        PIC 9(8).
                  25  VOF-ZAHLW        PIC X.
                  25  VOF-VERTRSTAT    PIC X.
                  25  VOF-STORNOGR     PIC XX.
                  25  VOF-STORNODAT    PIC 9(8).
                  25  VOF-AENDGR       PIC XX     OCCURS 5.
                  25  VOF-AENDDAT      PIC 9(8).
                  25  VOF-VERTRHW      PIC X.
                  25  VOF-ANTEILPROZ   PIC 9(3)V99.
                  25  VOF-AUFTRNR      PIC X(9).
                  25  VOF-WAEHRKZ      PIC X(3).
                  25  VOF-BEITRAG      PIC 9(10)V99.
                  25  VOF-MZF          PIC X(30).
                  25  VOF-VSSTKZ       PIC X.
                  25  VOF-FREI-0200    PIC X(98).
CCC009*    SKIP1
               20  VOF-VERTRAG-SPEZ01  REDEFINES  VOF-ADRESS-TEIL.
                  25  VOF-ENTSTKZ      PIC X.
                  25  VOF-WAEHRKZ-2    PIC X(3).
                  25  VOF-BTRDEPOT     PIC X.
                  25  VOF-RISIKOVERL   PIC X.
                  25  VOF-VORLSU       PIC 9(12).
                  25  VOF-VBTRZDAUER   PIC 99.
                  25  VOF-VERWENDUNG   PIC X.
                  25  VOF-VERTRFORM    PIC X.
                  25  VOF-GRUART       PIC X(40).
                  25  VOF-NAME-VPK     PIC X(20).
                  25  VOF-PROVISION    PIC 9(3)V99.
                  25  VOF-PROVKZ       PIC X.
                  25  VOF-BESTPFL      PIC 9(5).
                  25  VOF-BESTPFLKZ    PIC X.
                  25  VOF-FREI-0210    PIC X(120).
CCC009*    SKIP1
               20  VOF-VERTRAG-SPARTE01 REDEFINES  VOF-ADRESS-TEIL.
                  25  VOF-LFDNR-VP     PIC X(17).
                  25  VOF-WAGNIS       PIC X.
                  25  VOF-WAGNISNR     PIC X.
                  25  VOF-VERTRAG-PERS.
                    30  VOF-NAME-VP    PIC X(30).
                    30  VOF-VNAME-VP   PIC X(30).
                    30  VOF-GEBD-VP    PIC 9(8).
                    30  VOF-GESCHL-VP  PIC X.
                    30  VOF-BERUF      PIC X(3).
                    30  VOF-BERUFTXT   PIC X(30).
                    30  VOF-ANZAHLVP   PIC 9.
                    30  VOF-GEBD-VP2   PIC 9(8).
                    30  VOF-FREI-0221  PIC X(84).
CCC009*    SKIP1
                  25  VOF-VERTRAG-ART  REDEFINES  VOF-VERTRAG-PERS.
                    30  VOF-VBEG2          PIC 9(8).
                    30  VOF-VABL2          PIC 9(8).
                    30  VOF-AEND2          PIC 9(8).
                    30  VOF-VLAUFZEIT      PIC 9(3).
                    30  VOF-VART           PIC X.
                    30  VOF-BEITRAG-B      PIC 9(10)V99.
                    30  VOF-ERLF-VS        PIC 9(9).
                    30  VOF-TODF-VS        PIC 9(9).
                    30  VOF-FALLKZ         PIC X.
                    30  VOF-BEITRAG-N      PIC 9(10)V99.
                    30  VOF-ERLF-VS2       PIC 9(9).
                    30  VOF-TARIF          PIC X(20).
                    30  VOF-STATUS         PIC X.
                    30  VOF-BFERLF-VS      PIC 9(9).
                    30  VOF-BFTODF-VS      PIC 9(9).
                    30  VOF-LFZK           PIC X.
                    30  VOF-GVA            PIC X.
                    30  VOF-UEBERAB        PIC 9(8).
                    30  VOF-EINSCHR        PIC X.
                    30  VOF-ZUSCHLAG       PIC X.
                    30  VOF-DYNAMIK        PIC 9(3)V99.
                    30  VOF-BASISERH       PIC X.
                    30  VOF-ARTERH         PIC X.
                    30  VOF-DYNSTOP        PIC 9(8).
                    30  VOF-DYNDAT         PIC 9(8).
                    30  VOF-ALTERENDE      PIC 9(3).
                    30  VOF-ALTEREINTR     PIC 9(2).
                    30  VOF-ZUWACHSKZ      PIC X.
                    30  VOF-RUECKKWERT     PIC 9(9).
                    30  VOF-RUECKKAB       PIC 9(6).
                    30  VOF-GUTHABEN       PIC 9(9).
                    30  VOF-UMTAUSCHKZ     PIC X.
                    30  VOF-SPARKZ         PIC X.
                    30  VOF-RUHBIS         PIC 9(8).
                    30  VOF-FREI-0222      PIC X.
