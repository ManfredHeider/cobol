      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU185S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU185S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:55:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *******************************************************
      ****  COPY -MEMBER FUER VERMOEGENSBERATER-STATISTIK  **
      *******************************************************
      *
       01  VSATZ.
           05  VSATZ1.
               10  VSVBNR         PIC 9(7)     COMP-3.
               10  VSDATUM        PIC 9(6)     COMP-3.
               10  VSMDAT         PIC X(5).
               10  VSJDAT         PIC X(10).
               10  VSQDAT         PIC X(10).
      *
      * G E S A M T E I N H E I T E N
      *
          05  GESAMT.
              10  GSZUGEL        PIC S9(7)V99         COMP-3.
              10  GSZUGBL        PIC S9(7)V99         COMP-3.
              10  GSABGEL        PIC S9(7)V99         COMP-3.
              10  GSABGBL        PIC S9(7)V99         COMP-3.
              10  GSNETEL        PIC S9(7)V99         COMP-3.
              10  GSNETBL        PIC S9(7)V99         COMP-3.
              10  GSSTOEL        PIC S9(7)V99         COMP-3.
              10  GSSTOBL        PIC S9(7)V99         COMP-3.
              10  GSSTOGL        PIC S9(7)V99         COMP-3.
              10  GSZUGEJ        PIC S9(7)V99         COMP-3.
              10  GSZUGBJ        PIC S9(7)V99         COMP-3.
              10  GSABGEJ        PIC S9(7)V99         COMP-3.
              10  GSABGBJ        PIC S9(7)V99         COMP-3.
              10  GSNETEJ        PIC S9(7)V99         COMP-3.
              10  GSNETBJ        PIC S9(7)V99         COMP-3.
              10  GSSTOEJ        PIC S9(7)V99         COMP-3.
              10  GSSTOBJ        PIC S9(7)V99         COMP-3.
              10  GSSTOGJ        PIC S9(7)V99         COMP-3.
              10  GSZUGEQ        PIC S9(7)V99         COMP-3.
              10  GSZUGBQ        PIC S9(7)V99         COMP-3.
              10  GSABGEQ        PIC S9(7)V99         COMP-3.
              10  GSABGBQ        PIC S9(7)V99         COMP-3.
              10  GSNETEQ        PIC S9(7)V99         COMP-3.
              10  GSNETBQ        PIC S9(7)V99         COMP-3.
              10  GSSTOEQ        PIC S9(7)V99         COMP-3.
              10  GSSTOBQ        PIC S9(7)V99         COMP-3.
              10  GSSTOGQ        PIC S9(7)V99         COMP-3.
      *
      *   L E B E N S V E R S I C H E R U N G
      *
          05  LEBEN.
              10  LEZUGEL        PIC S9(7)V99         COMP-3.
              10  LEZUGBL        PIC S9(7)V99         COMP-3.
              10  LEABGEL        PIC S9(7)V99         COMP-3.
              10  LEABGBL        PIC S9(7)V99         COMP-3.
              10  LENETEL        PIC S9(7)V99         COMP-3.
              10  LENETBL        PIC S9(7)V99         COMP-3.
              10  LESTOEL        PIC S9(7)V99         COMP-3.
              10  LESTOBL        PIC S9(7)V99         COMP-3.
              10  LESTOGL        PIC S9(7)V99         COMP-3.
              10  LEZUGEJ        PIC S9(7)V99         COMP-3.
              10  LEZUGBJ        PIC S9(7)V99         COMP-3.
              10  LEABGEJ        PIC S9(7)V99         COMP-3.
              10  LEABGBJ        PIC S9(7)V99         COMP-3.
              10  LENETEJ        PIC S9(7)V99         COMP-3.
              10  LENETBJ        PIC S9(7)V99         COMP-3.
              10  LESTOEJ        PIC S9(7)V99         COMP-3.
              10  LESTOBJ        PIC S9(7)V99         COMP-3.
              10  LESTOGJ        PIC S9(7)V99         COMP-3.
              10  LEZUGEQ        PIC S9(7)V99         COMP-3.
              10  LEZUGBQ        PIC S9(7)V99         COMP-3.
              10  LEABGEQ        PIC S9(7)V99         COMP-3.
              10  LEABGBQ        PIC S9(7)V99         COMP-3.
              10  LENETEQ        PIC S9(7)V99         COMP-3.
              10  LENETBQ        PIC S9(7)V99         COMP-3.
              10  LESTOEQ        PIC S9(7)V99         COMP-3.
              10  LESTOBQ        PIC S9(7)V99         COMP-3.
              10  LESTOGQ        PIC S9(7)V99         COMP-3.
      *
          05  DYNAMIKQ.
              10  DYNQEL         PIC S9(7)V99         COMP-3.
              10  DYNQBL         PIC S9(7)V99         COMP-3.
              10  DYNQGL         PIC S9(7)V99         COMP-3.
              10  DYNQEJ         PIC S9(7)V99         COMP-3.
              10  DYNQBJ         PIC S9(7)V99         COMP-3.
              10  DYNQGJ         PIC S9(7)V99         COMP-3.
              10  DYNQEQ         PIC S9(7)V99         COMP-3.
              10  DYNQBQ         PIC S9(7)V99         COMP-3.
              10  DYNQGQ         PIC S9(7)V99         COMP-3.
      *
          05  PR-RATA.
              10  PRESEL         PIC S9(7)V99         COMP-3.
              10  PRESBL         PIC S9(7)V99         COMP-3.
              10  PRESGL         PIC S9(7)V99         COMP-3.
              10  PRESEJ         PIC S9(7)V99         COMP-3.
              10  PRESBJ         PIC S9(7)V99         COMP-3.
              10  PRESGJ         PIC S9(7)V99         COMP-3.
              10  PRESEQ         PIC S9(7)V99         COMP-3.
              10  PRESBQ         PIC S9(7)V99         COMP-3.
              10  PRESGQ         PIC S9(7)V99         COMP-3.
      *
          05  STO-RUE.
              10  STRPFEL        PIC S9(7)V99         COMP-3.
              10  STRPFBL        PIC S9(7)V99         COMP-3.
              10  STRPFGL        PIC S9(7)V99         COMP-3.
              10  STRPFEJ        PIC S9(7)V99         COMP-3.
              10  STRPFBJ        PIC S9(7)V99         COMP-3.
              10  STRPFGJ        PIC S9(7)V99         COMP-3.
              10  STRPFEQ        PIC S9(7)V99         COMP-3.
              10  STRPFBQ        PIC S9(7)V99         COMP-3.
              10  STRPFGQ        PIC S9(7)V99         COMP-3.
      *
          05  PROD-GESAMT.
              10  PEGSE-BIS      PIC S9(7)V99         COMP-3.
              10  PEGSB-BIS      PIC S9(7)V99         COMP-3.
              10  PEGSE-AB       PIC S9(7)V99         COMP-3.
              10  PEGSB-AB       PIC S9(7)V99         COMP-3.
      *
      *   P R O V I S I O N S V E R D I E N S T
      *
          05  AM-LEBEN.
              10  AMLEL          PIC S9(7)V99         COMP-3.
              10  AMLBL          PIC S9(7)V99         COMP-3.
              10  AMLEJ          PIC S9(7)V99         COMP-3.
              10  AMLBJ          PIC S9(7)V99         COMP-3.
              10  AMLEQ          PIC S9(7)V99         COMP-3.
              10  AMLBQ          PIC S9(7)V99         COMP-3.
      *
      *  COSMOS-LEBEN WIRD Z.ZT. NICHT BENUTZT (SCHORN 15.12.81)
      *
          05  COSMOS.
              10  COLEL          PIC S9(7)V99         COMP-3.
              10  COLBL          PIC S9(7)V99         COMP-3.
              10  COLEJ          PIC S9(7)V99         COMP-3.
              10  COLBJ          PIC S9(7)V99         COMP-3.
              10  COLEQ          PIC S9(7)V99         COMP-3.
              10  COLBQ          PIC S9(7)V99         COMP-3.
      *
          05  DYNAMIKERH.
              10  DYEEL          PIC S9(7)V99         COMP-3.
              10  DYEBL          PIC S9(7)V99         COMP-3.
              10  DYEEJ          PIC S9(7)V99         COMP-3.
              10  DYEBJ          PIC S9(7)V99         COMP-3.
              10  DYEEQ          PIC S9(7)V99         COMP-3.
              10  DYEBQ          PIC S9(7)V99         COMP-3.
      *
          05  AM-UNFALL.
              10  AMUEL          PIC S9(7)V99         COMP-3.
              10  AMUBL          PIC S9(7)V99         COMP-3.
              10  AMUEJ          PIC S9(7)V99         COMP-3.
              10  AMUBJ          PIC S9(7)V99         COMP-3.
              10  AMUEQ          PIC S9(7)V99         COMP-3.
              10  AMUBQ          PIC S9(7)V99         COMP-3.
      *
          05  AM-KFZ.
              10  AMKEL          PIC S9(7)V99         COMP-3.
              10  AMKBL          PIC S9(7)V99         COMP-3.
              10  AMKEJ          PIC S9(7)V99         COMP-3.
              10  AMKBJ          PIC S9(7)V99         COMP-3.
              10  AMKEQ          PIC S9(7)V99         COMP-3.
              10  AMKBQ          PIC S9(7)V99         COMP-3.
      *
          05  AM-SONST.
              10  AMSEL          PIC S9(7)V99         COMP-3.
              10  AMSBL          PIC S9(7)V99         COMP-3.
              10  AMSEJ          PIC S9(7)V99         COMP-3.
              10  AMSBJ          PIC S9(7)V99         COMP-3.
              10  AMSEQ          PIC S9(7)V99         COMP-3.
              10  AMSBQ          PIC S9(7)V99         COMP-3.
      *
          05  FOLGEPROV.
              10  FOPEL          PIC S9(7)V99         COMP-3.
              10  FOPBL          PIC S9(7)V99         COMP-3.
              10  FOPEJ          PIC S9(7)V99         COMP-3.
              10  FOPBJ          PIC S9(7)V99         COMP-3.
              10  FOPEQ          PIC S9(7)V99         COMP-3.
              10  FOPBQ          PIC S9(7)V99         COMP-3.
      *
          05  BADENIA.
              10  BADEL          PIC S9(7)V99         COMP-3.
              10  BADBL          PIC S9(7)V99         COMP-3.
              10  BADEJ          PIC S9(7)V99         COMP-3.
              10  BADBJ          PIC S9(7)V99         COMP-3.
              10  BADEQ          PIC S9(7)V99         COMP-3.
              10  BADBQ          PIC S9(7)V99         COMP-3.
      *
          05  CKV.
              10  CKVEL          PIC S9(7)V99         COMP-3.
              10  CKVBL          PIC S9(7)V99         COMP-3.
              10  CKVEJ          PIC S9(7)V99         COMP-3.
              10  CKVBJ          PIC S9(7)V99         COMP-3.
              10  CKVEQ          PIC S9(7)V99         COMP-3.
              10  CKVBQ          PIC S9(7)V99         COMP-3.
      *
          05  FONDS.
              10  FONEL          PIC S9(7)V99         COMP-3.
              10  FONBL          PIC S9(7)V99         COMP-3.
              10  FONEJ          PIC S9(7)V99         COMP-3.
              10  FONBJ          PIC S9(7)V99         COMP-3.
              10  FONEQ          PIC S9(7)V99         COMP-3.
              10  FONBQ          PIC S9(7)V99         COMP-3.
      *
          05  SONSTIGE.
              10  SONEL          PIC S9(7)V99         COMP-3.
              10  SONBL          PIC S9(7)V99         COMP-3.
              10  SONEJ          PIC S9(7)V99         COMP-3.
              10  SONBJ          PIC S9(7)V99         COMP-3.
              10  SONEQ          PIC S9(7)V99         COMP-3.
              10  SONBQ          PIC S9(7)V99         COMP-3.
      *
      * K L V
      *
          05  PROD-ART.
              10  KLZUGEL        PIC S9(8)V9          COMP-3.
              10  KLZUGBL        PIC S9(8)V9          COMP-3.
              10  KLABGEL        PIC S9(8)V9          COMP-3.
              10  KLABGBL        PIC S9(8)V9          COMP-3.
              10  KLSTOEL        PIC S9(8)V9          COMP-3.
              10  KLSTOBL        PIC S9(8)V9          COMP-3.
              10  KLSTOGL        PIC S9(8)V9          COMP-3.
              10  KLZUGEJ        PIC S9(8)V9          COMP-3.
              10  KLZUGBJ        PIC S9(8)V9          COMP-3.
              10  KLABGEJ        PIC S9(8)V9          COMP-3.
              10  KLABGBJ        PIC S9(8)V9          COMP-3.
              10  KLSTOEJ        PIC S9(8)V9          COMP-3.
              10  KLSTOBJ        PIC S9(8)V9          COMP-3.
              10  KLSTOGJ        PIC S9(8)V9          COMP-3.
              10  KLZUGEQ        PIC S9(8)V9          COMP-3.
              10  KLZUGBQ        PIC S9(8)V9          COMP-3.
              10  KLABGEQ        PIC S9(8)V9          COMP-3.
              10  KLABGBQ        PIC S9(8)V9          COMP-3.
              10  KLSTOEQ        PIC S9(8)V9          COMP-3.
              10  KLSTOBQ        PIC S9(8)V9          COMP-3.
              10  KLSTOGQ        PIC S9(8)V9          COMP-3.
      *
      * C O
      *
              10  COZUGEL        PIC S9(8)V9          COMP-3.
              10  COZUGBL        PIC S9(8)V9          COMP-3.
              10  COABGEL        PIC S9(8)V9          COMP-3.
              10  COABGBL        PIC S9(8)V9          COMP-3.
              10  COSTOEL        PIC S9(8)V9          COMP-3.
              10  COSTOBL        PIC S9(8)V9          COMP-3.
              10  COSTOGL        PIC S9(8)V9          COMP-3.
              10  COZUGEJ        PIC S9(8)V9          COMP-3.
              10  COZUGBJ        PIC S9(8)V9          COMP-3.
              10  COABGEJ        PIC S9(8)V9          COMP-3.
              10  COABGBJ        PIC S9(8)V9          COMP-3.
              10  COSTOEJ        PIC S9(8)V9          COMP-3.
              10  COSTOBJ        PIC S9(8)V9          COMP-3.
              10  COSTOGJ        PIC S9(8)V9          COMP-3.
              10  COZUGEQ        PIC S9(8)V9          COMP-3.
              10  COZUGBQ        PIC S9(8)V9          COMP-3.
              10  COABGEQ        PIC S9(8)V9          COMP-3.
              10  COABGBQ        PIC S9(8)V9          COMP-3.
              10  COSTOEQ        PIC S9(8)V9          COMP-3.
              10  COSTOBQ        PIC S9(8)V9          COMP-3.
              10  COSTOGQ        PIC S9(8)V9          COMP-3.
      *
      * F L V
      *
              10  FLZUGEL        PIC S9(8)V9          COMP-3.
              10  FLZUGBL        PIC S9(8)V9          COMP-3.
              10  FLABGEL        PIC S9(8)V9          COMP-3.
              10  FLABGBL        PIC S9(8)V9          COMP-3.
              10  FLSTOEL        PIC S9(8)V9          COMP-3.
              10  FLSTOBL        PIC S9(8)V9          COMP-3.
              10  FLSTOGL        PIC S9(8)V9          COMP-3.
              10  FLZUGEJ        PIC S9(8)V9          COMP-3.
              10  FLZUGBJ        PIC S9(8)V9          COMP-3.
              10  FLABGEJ        PIC S9(8)V9          COMP-3.
              10  FLABGBJ        PIC S9(8)V9          COMP-3.
              10  FLSTOEJ        PIC S9(8)V9          COMP-3.
              10  FLSTOBJ        PIC S9(8)V9          COMP-3.
              10  FLSTOGJ        PIC S9(8)V9          COMP-3.
              10  FLZUGEQ        PIC S9(8)V9          COMP-3.
              10  FLZUGBQ        PIC S9(8)V9          COMP-3.
              10  FLABGEQ        PIC S9(8)V9          COMP-3.
              10  FLABGBQ        PIC S9(8)V9          COMP-3.
              10  FLSTOEQ        PIC S9(8)V9          COMP-3.
              10  FLSTOBQ        PIC S9(8)V9          COMP-3.
              10  FLSTOGQ        PIC S9(8)V9          COMP-3.
      *
      * D V P
      *
              10  DVZUGEL        PIC S9(8)V9          COMP-3.
              10  DVZUGBL        PIC S9(8)V9          COMP-3.
              10  DVABGEL        PIC S9(8)V9          COMP-3.
              10  DVABGBL        PIC S9(8)V9          COMP-3.
              10  DVSTOEL        PIC S9(8)V9          COMP-3.
              10  DVSTOBL        PIC S9(8)V9          COMP-3.
              10  DVSTOGL        PIC S9(8)V9          COMP-3.
              10  DVZUGEJ        PIC S9(8)V9          COMP-3.
              10  DVZUGBJ        PIC S9(8)V9          COMP-3.
              10  DVABGEJ        PIC S9(8)V9          COMP-3.
              10  DVABGBJ        PIC S9(8)V9          COMP-3.
              10  DVSTOEJ        PIC S9(8)V9          COMP-3.
              10  DVSTOBJ        PIC S9(8)V9          COMP-3.
              10  DVSTOGJ        PIC S9(8)V9          COMP-3.
              10  DVZUGEQ        PIC S9(8)V9          COMP-3.
              10  DVZUGBQ        PIC S9(8)V9          COMP-3.
              10  DVABGEQ        PIC S9(8)V9          COMP-3.
              10  DVABGBQ        PIC S9(8)V9          COMP-3.
              10  DVSTOEQ        PIC S9(8)V9          COMP-3.
              10  DVSTOBQ        PIC S9(8)V9          COMP-3.
              10  DVSTOGQ        PIC S9(8)V9          COMP-3.
      *
      * V L V
      *
              10  VLZUGEL        PIC S9(8)V9          COMP-3.
              10  VLZUGBL        PIC S9(8)V9          COMP-3.
              10  VLABGEL        PIC S9(8)V9          COMP-3.
              10  VLABGBL        PIC S9(8)V9          COMP-3.
              10  VLSTOEL        PIC S9(8)V9          COMP-3.
              10  VLSTOBL        PIC S9(8)V9          COMP-3.
              10  VLSTOGL        PIC S9(8)V9          COMP-3.
              10  VLZUGEJ        PIC S9(8)V9          COMP-3.
              10  VLZUGBJ        PIC S9(8)V9          COMP-3.
              10  VLABGEJ        PIC S9(8)V9          COMP-3.
              10  VLABGBJ        PIC S9(8)V9          COMP-3.
              10  VLSTOEJ        PIC S9(8)V9          COMP-3.
              10  VLSTOBJ        PIC S9(8)V9          COMP-3.
              10  VLSTOGJ        PIC S9(8)V9          COMP-3.
              10  VLZUGEQ        PIC S9(8)V9          COMP-3.
              10  VLZUGBQ        PIC S9(8)V9          COMP-3.
              10  VLABGEQ        PIC S9(8)V9          COMP-3.
              10  VLABGBQ        PIC S9(8)V9          COMP-3.
              10  VLSTOEQ        PIC S9(8)V9          COMP-3.
              10  VLSTOBQ        PIC S9(8)V9          COMP-3.
              10  VLSTOGQ        PIC S9(8)V9          COMP-3.
      *
      * S O N S T I G E   L E B E N
      *
              10  SOZUGEL        PIC S9(8)V9          COMP-3.
              10  SOZUGBL        PIC S9(8)V9          COMP-3.
              10  SOABGEL        PIC S9(8)V9          COMP-3.
              10  SOABGBL        PIC S9(8)V9          COMP-3.
              10  SOSTOEL        PIC S9(8)V9          COMP-3.
              10  SOSTOBL        PIC S9(8)V9          COMP-3.
              10  SOSTOGL        PIC S9(8)V9          COMP-3.
              10  SOZUGEJ        PIC S9(8)V9          COMP-3.
              10  SOZUGBJ        PIC S9(8)V9          COMP-3.
              10  SOABGEJ        PIC S9(8)V9          COMP-3.
              10  SOABGBJ        PIC S9(8)V9          COMP-3.
              10  SOSTOEJ        PIC S9(8)V9          COMP-3.
              10  SOSTOBJ        PIC S9(8)V9          COMP-3.
              10  SOSTOGJ        PIC S9(8)V9          COMP-3.
              10  SOZUGEQ        PIC S9(8)V9          COMP-3.
              10  SOZUGBQ        PIC S9(8)V9          COMP-3.
              10  SOABGEQ        PIC S9(8)V9          COMP-3.
              10  SOABGBQ        PIC S9(8)V9          COMP-3.
              10  SOSTOEQ        PIC S9(8)V9          COMP-3.
              10  SOSTOBQ        PIC S9(8)V9          COMP-3.
              10  SOSTOGQ        PIC S9(8)V9          COMP-3.
      *
      * U V
      *
              10  UVZUGEL        PIC S9(8)V9          COMP-3.
              10  UVZUGBL        PIC S9(8)V9          COMP-3.
              10  UVABGEL        PIC S9(8)V9          COMP-3.
              10  UVABGBL        PIC S9(8)V9          COMP-3.
              10  UVSTOEL        PIC S9(8)V9          COMP-3.
              10  UVSTOBL        PIC S9(8)V9          COMP-3.
              10  UVSTOGL        PIC S9(8)V9          COMP-3.
              10  UVZUGEJ        PIC S9(8)V9          COMP-3.
              10  UVZUGBJ        PIC S9(8)V9          COMP-3.
              10  UVABGEJ        PIC S9(8)V9          COMP-3.
              10  UVABGBJ        PIC S9(8)V9          COMP-3.
              10  UVSTOEJ        PIC S9(8)V9          COMP-3.
              10  UVSTOBJ        PIC S9(8)V9          COMP-3.
              10  UVSTOGJ        PIC S9(8)V9          COMP-3.
              10  UVZUGEQ        PIC S9(8)V9          COMP-3.
              10  UVZUGBQ        PIC S9(8)V9          COMP-3.
              10  UVABGEQ        PIC S9(8)V9          COMP-3.
              10  UVABGBQ        PIC S9(8)V9          COMP-3.
              10  UVSTOEQ        PIC S9(8)V9          COMP-3.
              10  UVSTOBQ        PIC S9(8)V9          COMP-3.
              10  UVSTOGQ        PIC S9(8)V9          COMP-3.
      *
      * K F Z
      *
              10  KFZUGEL        PIC S9(8)V9          COMP-3.
              10  KFZUGBL        PIC S9(8)V9          COMP-3.
              10  KFABGEL        PIC S9(8)V9          COMP-3.
              10  KFABGBL        PIC S9(8)V9          COMP-3.
              10  KFSTOEL        PIC S9(8)V9          COMP-3.
              10  KFSTOBL        PIC S9(8)V9          COMP-3.
              10  KFSTOGL        PIC S9(8)V9          COMP-3.
              10  KFZUGEJ        PIC S9(8)V9          COMP-3.
              10  KFZUGBJ        PIC S9(8)V9          COMP-3.
              10  KFABGEJ        PIC S9(8)V9          COMP-3.
              10  KFABGBJ        PIC S9(8)V9          COMP-3.
              10  KFSTOEJ        PIC S9(8)V9          COMP-3.
              10  KFSTOBJ        PIC S9(8)V9          COMP-3.
              10  KFSTOGJ        PIC S9(8)V9          COMP-3.
              10  KFZUGEQ        PIC S9(8)V9          COMP-3.
              10  KFZUGBQ        PIC S9(8)V9          COMP-3.
              10  KFABGEQ        PIC S9(8)V9          COMP-3.
              10  KFABGBQ        PIC S9(8)V9          COMP-3.
              10  KFSTOEQ        PIC S9(8)V9          COMP-3.
              10  KFSTOBQ        PIC S9(8)V9          COMP-3.
              10  KFSTOGQ        PIC S9(8)V9          COMP-3.
      *
      * S V
      *
              10  SVZUGEL        PIC S9(8)V9          COMP-3.
              10  SVZUGBL        PIC S9(8)V9          COMP-3.
              10  SVABGEL        PIC S9(8)V9          COMP-3.
              10  SVABGBL        PIC S9(8)V9          COMP-3.
              10  SVSTOEL        PIC S9(8)V9          COMP-3.
              10  SVSTOBL        PIC S9(8)V9          COMP-3.
              10  SVSTOGL        PIC S9(8)V9          COMP-3.
              10  SVZUGEJ        PIC S9(8)V9          COMP-3.
              10  SVZUGBJ        PIC S9(8)V9          COMP-3.
              10  SVABGEJ        PIC S9(8)V9          COMP-3.
              10  SVABGBJ        PIC S9(8)V9          COMP-3.
              10  SVSTOEJ        PIC S9(8)V9          COMP-3.
              10  SVSTOBJ        PIC S9(8)V9          COMP-3.
              10  SVSTOGJ        PIC S9(8)V9          COMP-3.
              10  SVZUGEQ        PIC S9(8)V9          COMP-3.
              10  SVZUGBQ        PIC S9(8)V9          COMP-3.
              10  SVABGEQ        PIC S9(8)V9          COMP-3.
              10  SVABGBQ        PIC S9(8)V9          COMP-3.
              10  SVSTOEQ        PIC S9(8)V9          COMP-3.
              10  SVSTOBQ        PIC S9(8)V9          COMP-3.
              10  SVSTOGQ        PIC S9(8)V9          COMP-3.
      *
      * R S
      *
              10  RSZUGEL        PIC S9(8)V9          COMP-3.
              10  RSZUGBL        PIC S9(8)V9          COMP-3.
              10  RSABGEL        PIC S9(8)V9          COMP-3.
              10  RSABGBL        PIC S9(8)V9          COMP-3.
              10  RSSTOEL        PIC S9(8)V9          COMP-3.
              10  RSSTOBL        PIC S9(8)V9          COMP-3.
              10  RSSTOGL        PIC S9(8)V9          COMP-3.
              10  RSZUGEJ        PIC S9(8)V9          COMP-3.
              10  RSZUGBJ        PIC S9(8)V9          COMP-3.
              10  RSABGEJ        PIC S9(8)V9          COMP-3.
              10  RSABGBJ        PIC S9(8)V9          COMP-3.
              10  RSSTOEJ        PIC S9(8)V9          COMP-3.
              10  RSSTOBJ        PIC S9(8)V9          COMP-3.
              10  RSSTOGJ        PIC S9(8)V9          COMP-3.
              10  RSZUGEQ        PIC S9(8)V9          COMP-3.
              10  RSZUGBQ        PIC S9(8)V9          COMP-3.
              10  RSABGEQ        PIC S9(8)V9          COMP-3.
              10  RSABGBQ        PIC S9(8)V9          COMP-3.
              10  RSSTOEQ        PIC S9(8)V9          COMP-3.
              10  RSSTOBQ        PIC S9(8)V9          COMP-3.
              10  RSSTOGQ        PIC S9(8)V9          COMP-3.
      *
      * B S
      *
              10  BSZUGEL        PIC S9(8)V9          COMP-3.
              10  BSZUGBL        PIC S9(8)V9          COMP-3.
              10  BSABGEL        PIC S9(8)V9          COMP-3.
              10  BSABGBL        PIC S9(8)V9          COMP-3.
              10  BSSTOEL        PIC S9(8)V9          COMP-3.
              10  BSSTOBL        PIC S9(8)V9          COMP-3.
              10  BSSTOGL        PIC S9(8)V9          COMP-3.
              10  BSZUGEJ        PIC S9(8)V9          COMP-3.
              10  BSZUGBJ        PIC S9(8)V9          COMP-3.
              10  BSABGEJ        PIC S9(8)V9          COMP-3.
              10  BSABGBJ        PIC S9(8)V9          COMP-3.
              10  BSSTOEJ        PIC S9(8)V9          COMP-3.
              10  BSSTOBJ        PIC S9(8)V9          COMP-3.
              10  BSSTOGJ        PIC S9(8)V9          COMP-3.
              10  BSZUGEQ        PIC S9(8)V9          COMP-3.
              10  BSZUGBQ        PIC S9(8)V9          COMP-3.
              10  BSABGEQ        PIC S9(8)V9          COMP-3.
              10  BSABGBQ        PIC S9(8)V9          COMP-3.
              10  BSSTOEQ        PIC S9(8)V9          COMP-3.
              10  BSSTOBQ        PIC S9(8)V9          COMP-3.
              10  BSSTOGQ        PIC S9(8)V9          COMP-3.
      *
      * K V
      *
              10  KVZUGEL        PIC S9(8)V9          COMP-3.
              10  KVZUGBL        PIC S9(8)V9          COMP-3.
              10  KVABGEL        PIC S9(8)V9          COMP-3.
              10  KVABGBL        PIC S9(8)V9          COMP-3.
              10  KVSTOEL        PIC S9(8)V9          COMP-3.
              10  KVSTOBL        PIC S9(8)V9          COMP-3.
              10  KVSTOGL        PIC S9(8)V9          COMP-3.
              10  KVZUGEJ        PIC S9(8)V9          COMP-3.
              10  KVZUGBJ        PIC S9(8)V9          COMP-3.
              10  KVABGEJ        PIC S9(8)V9          COMP-3.
              10  KVABGBJ        PIC S9(8)V9          COMP-3.
              10  KVSTOEJ        PIC S9(8)V9          COMP-3.
              10  KVSTOBJ        PIC S9(8)V9          COMP-3.
              10  KVSTOGJ        PIC S9(8)V9          COMP-3.
              10  KVZUGEQ        PIC S9(8)V9          COMP-3.
              10  KVZUGBQ        PIC S9(8)V9          COMP-3.
              10  KVABGEQ        PIC S9(8)V9          COMP-3.
              10  KVABGBQ        PIC S9(8)V9          COMP-3.
              10  KVSTOEQ        PIC S9(8)V9          COMP-3.
              10  KVSTOBQ        PIC S9(8)V9          COMP-3.
              10  KVSTOGQ        PIC S9(8)V9          COMP-3.
      *
      * D I T
      *
              10  DIZUGEL        PIC S9(8)V9          COMP-3.
              10  DIZUGBL        PIC S9(8)V9          COMP-3.
              10  DIABGEL        PIC S9(8)V9          COMP-3.
              10  DIABGBL        PIC S9(8)V9          COMP-3.
              10  DISTOEL        PIC S9(8)V9          COMP-3.
              10  DISTOBL        PIC S9(8)V9          COMP-3.
              10  DISTOGL        PIC S9(8)V9          COMP-3.
              10  DIZUGEJ        PIC S9(8)V9          COMP-3.
              10  DIZUGBJ        PIC S9(8)V9          COMP-3.
              10  DIABGEJ        PIC S9(8)V9          COMP-3.
              10  DIABGBJ        PIC S9(8)V9          COMP-3.
              10  DISTOEJ        PIC S9(8)V9          COMP-3.
              10  DISTOBJ        PIC S9(8)V9          COMP-3.
              10  DISTOGJ        PIC S9(8)V9          COMP-3.
              10  DIZUGEQ        PIC S9(8)V9          COMP-3.
              10  DIZUGBQ        PIC S9(8)V9          COMP-3.
              10  DIABGEQ        PIC S9(8)V9          COMP-3.
              10  DIABGBQ        PIC S9(8)V9          COMP-3.
              10  DISTOEQ        PIC S9(8)V9          COMP-3.
              10  DISTOBQ        PIC S9(8)V9          COMP-3.
              10  DISTOGQ        PIC S9(8)V9          COMP-3.
      *
      * I I I
      *
              10  IIZUGEL        PIC S9(8)V9          COMP-3.
              10  IIZUGBL        PIC S9(8)V9          COMP-3.
              10  IIABGEL        PIC S9(8)V9          COMP-3.
              10  IIABGBL        PIC S9(8)V9          COMP-3.
              10  IISTOEL        PIC S9(8)V9          COMP-3.
              10  IISTOBL        PIC S9(8)V9          COMP-3.
              10  IISTOGL        PIC S9(8)V9          COMP-3.
              10  IIZUGEJ        PIC S9(8)V9          COMP-3.
              10  IIZUGBJ        PIC S9(8)V9          COMP-3.
              10  IIABGEJ        PIC S9(8)V9          COMP-3.
              10  IIABGBJ        PIC S9(8)V9          COMP-3.
              10  IISTOEJ        PIC S9(8)V9          COMP-3.
              10  IISTOBJ        PIC S9(8)V9          COMP-3.
              10  IISTOGJ        PIC S9(8)V9          COMP-3.
              10  IIZUGEQ        PIC S9(8)V9          COMP-3.
              10  IIZUGBQ        PIC S9(8)V9          COMP-3.
              10  IIABGEQ        PIC S9(8)V9          COMP-3.
              10  IIABGBQ        PIC S9(8)V9          COMP-3.
              10  IISTOEQ        PIC S9(8)V9          COMP-3.
              10  IISTOBQ        PIC S9(8)V9          COMP-3.
              10  IISTOGQ        PIC S9(8)V9          COMP-3.
      *
      * K A
      *
              10  KAZUGEL        PIC S9(8)V9          COMP-3.
              10  KAZUGBL        PIC S9(8)V9          COMP-3.
              10  KAABGEL        PIC S9(8)V9          COMP-3.
              10  KAABGBL        PIC S9(8)V9          COMP-3.
              10  KASTOEL        PIC S9(8)V9          COMP-3.
              10  KASTOBL        PIC S9(8)V9          COMP-3.
              10  KASTOGL        PIC S9(8)V9          COMP-3.
              10  KAZUGEJ        PIC S9(8)V9          COMP-3.
              10  KAZUGBJ        PIC S9(8)V9          COMP-3.
              10  KAABGEJ        PIC S9(8)V9          COMP-3.
              10  KAABGBJ        PIC S9(8)V9          COMP-3.
              10  KASTOEJ        PIC S9(8)V9          COMP-3.
              10  KASTOBJ        PIC S9(8)V9          COMP-3.
              10  KASTOGJ        PIC S9(8)V9          COMP-3.
              10  KAZUGEQ        PIC S9(8)V9          COMP-3.
              10  KAZUGBQ        PIC S9(8)V9          COMP-3.
              10  KAABGEQ        PIC S9(8)V9          COMP-3.
              10  KAABGBQ        PIC S9(8)V9          COMP-3.
              10  KASTOEQ        PIC S9(8)V9          COMP-3.
              10  KASTOBQ        PIC S9(8)V9          COMP-3.
              10  KASTOGQ        PIC S9(8)V9          COMP-3.
      *
      * S P V
      *
              10  SPZUGEL        PIC S9(8)V9          COMP-3.
              10  SPZUGBL        PIC S9(8)V9          COMP-3.
              10  SPABGEL        PIC S9(8)V9          COMP-3.
              10  SPABGBL        PIC S9(8)V9          COMP-3.
              10  SPSTOEL        PIC S9(8)V9          COMP-3.
              10  SPSTOBL        PIC S9(8)V9          COMP-3.
              10  SPSTOGL        PIC S9(8)V9          COMP-3.
              10  SPZUGEJ        PIC S9(8)V9          COMP-3.
              10  SPZUGBJ        PIC S9(8)V9          COMP-3.
              10  SPABGEJ        PIC S9(8)V9          COMP-3.
              10  SPABGBJ        PIC S9(8)V9          COMP-3.
              10  SPSTOEJ        PIC S9(8)V9          COMP-3.
              10  SPSTOBJ        PIC S9(8)V9          COMP-3.
              10  SPSTOGJ        PIC S9(8)V9          COMP-3.
              10  SPZUGEQ        PIC S9(8)V9          COMP-3.
              10  SPZUGBQ        PIC S9(8)V9          COMP-3.
              10  SPABGEQ        PIC S9(8)V9          COMP-3.
              10  SPABGBQ        PIC S9(8)V9          COMP-3.
              10  SPSTOEQ        PIC S9(8)V9          COMP-3.
              10  SPSTOBQ        PIC S9(8)V9          COMP-3.
              10  SPSTOGQ        PIC S9(8)V9          COMP-3.
      *
      * S O N S T I G E
      *
              10  STZUGEL        PIC S9(8)V9          COMP-3.
              10  STZUGBL        PIC S9(8)V9          COMP-3.
              10  STABGEL        PIC S9(8)V9          COMP-3.
              10  STABGBL        PIC S9(8)V9          COMP-3.
              10  STSTOEL        PIC S9(8)V9          COMP-3.
              10  STSTOBL        PIC S9(8)V9          COMP-3.
              10  STSTOGL        PIC S9(8)V9          COMP-3.
              10  STZUGEJ        PIC S9(8)V9          COMP-3.
              10  STZUGBJ        PIC S9(8)V9          COMP-3.
              10  STABGEJ        PIC S9(8)V9          COMP-3.
              10  STABGBJ        PIC S9(8)V9          COMP-3.
              10  STSTOEJ        PIC S9(8)V9          COMP-3.
              10  STSTOBJ        PIC S9(8)V9          COMP-3.
              10  STSTOGJ        PIC S9(8)V9          COMP-3.
              10  STZUGEQ        PIC S9(8)V9          COMP-3.
              10  STZUGBQ        PIC S9(8)V9          COMP-3.
              10  STABGEQ        PIC S9(8)V9          COMP-3.
              10  STABGBQ        PIC S9(8)V9          COMP-3.
              10  STSTOEQ        PIC S9(8)V9          COMP-3.
              10  STSTOGQ        PIC S9(8)V9          COMP-3.
              10  STSTOBQ        PIC S9(8)V9          COMP-3.
      *
          05  KONTOST.
              10  KOPROV         PIC S9(7)V99         COMP-3.
              10  PROVRSA        PIC S9(8)V9          COMP-3.
      *
      *  E R G O L G S P R O V I S I O N
      *
          05  ERF-PROV.
              10  AEMDP          PIC S9(8)V9          COMP-3.
              10  DMBEP          PIC S9(7)V99         COMP-3.
              10  STQUP          PIC S9(7)V99         COMP-3.
              10  STMIP          PIC S9(7)V99         COMP-3.
              10  ERFP           PIC S9(7)V99         COMP-3.
      *
      *  E R F O L G S B O N U S
      *
          05  ERF-BONUS.
              10  AEMDB          PIC S9(8)V9          COMP-3.
              10  DMBEB          PIC S9(7)V99         COMP-3.
              10  STQUB          PIC S9(7)V99         COMP-3.
              10  ERFB           PIC S9(7)V99         COMP-3.
              10  UEBW           PIC S9(7)V99         COMP-3.
      *
      *  R E S E R V E
      *
          05  FILLER             PIC X(1367).

