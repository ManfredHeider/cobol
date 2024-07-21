      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU195S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU195S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:55:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   4    16.06.94 08:57:12
      *************************************************************
      ****      VB-STATISTIK-BAND AUS AACHEN / MONATL. WERTE   ****
      ****      ERWEITERT UM DDR-GESCHAEFT                     ****
      ****      ERWEITERT UM VM-ANTEIL  18.01.91               ****
      ****         VAR. SATZLAENGE MIT 1550 MAX.               ****
      ****                                                     ****
      ****      ERWEITERT UM:                                  ****
      ****      -BERUFSUNFAEHIGKEIT                            ****
      ****      -KONTOSTAND DISKONTKONTO                       ****
      ****      -KONTOSTAND VORSCHUSS                          ****
      ****      -UEBERWEISUNGSBETRAG AKT. VB-ABRECHNUNG        ****
      ****      -RUECKSTELLUNGSZUFLUSS                         ****
      ****      NEUE VAR. SATZLAENGE = 1699 BYTE MAX.          ****
      ****      AM 30.06.93  SCHMIDT                           ****
      ****                                                     ****
      ****      ERWEITERT WEGEN AVBG (AFVB0099)                ****
      ****      NEUE VAR. SATZLAENGE = 2799 BYTE MAX.          ****
      ****      AM 08.04.94  SCHMIDT                           ****
      ****                                                     ****
      *************************************************************
      *---------------------------------------------------------------*
      *    V S A T Z   -  S C H L Ü S S E L
      *---------------------------------------------------------------*
       01  VSATZ.
              05  VSKEY.
                 10 VSVBNR                  PIC 9(07).
                 10 VSMDAT                  PIC 9(04).
          03  VSKEY-REST-DATEN.
                 10 VSMDAT-JRT              PIC 9(02).

      *---------------------------------------------------------------*
      *    V S A T Z   -  D A T E N
      *---------------------------------------------------------------*
              05 VSATZ-DATEN.
      *
      *** V S A T Z   -  K O N T O S T
      *
                 10 KONTOST.
                    15 KOPROV            PIC S9(9)V99     COMP-3.
                    15 KODISKO           PIC S9(9)V99     COMP-3.
                    15 KOVORSCH          PIC S9(9)V99     COMP-3.
                    15 PROVRSA           PIC S9(3)V9      COMP-3.
      *
      *** V S A T Z   -  L V  -  B E S T A N D S S T O R N O
      *

                 10 LV-BST.
                    15 LVBSTSTOE         PIC S9(3)V9      COMP-3.
                    15 LVBSTSTOB         PIC S9(3)V9      COMP-3.
                    15 LVBSTSTOG         PIC S9(3)V9      COMP-3.
      *
      *** V S A T Z   -  E R F O L G S P R O V I S I O N E N
      *
                 10 ERF-PROV.
                    15 AEMDP             PIC S9(5)V9      COMP-3.
                    15 DMBEP             PIC S9(6)V99     COMP-3.
                    15 STQUP             PIC S9(6)V99     COMP-3.
                    15 STMIP             PIC S9(6)V99     COMP-3.
                    15 ERFP              PIC S9(6)V99     COMP-3.
      *
      *** V S A T Z   -  E R F O L G S B O N U S
      *
                 10 ERF-BONUS.
                    15 AEMDB             PIC S9(5)V9      COMP-3.
                    15 DMBEB             PIC S9(6)V99     COMP-3.
                    15 STQUB             PIC S9(6)V99     COMP-3.
                    15 ERFB              PIC S9(6)V99     COMP-3.
      *
      *** ÜBERWEISUNGS-BETRAG AUS ERFOLGSBONUS - UND PROVISION
      *
                 10 UEBW                 PIC S9(6)V99     COMP-3.
      *
      *** ÜBERWEISUNGS-BETRAG
      *
                 10 UEBBETRAG            PIC S9(7)V99     COMP-3.
      *
      *** ZUFÜSSE AUF RÜCKSTELLUNGSKONTO
      *
                 10 RUECKZUFL            PIC S9(9)V99     COMP-3.
      *
      *** G E S A M T E I N H E I T E N
      *
                 10 GESAMT.
                    15 GSZUGEL           PIC S9(9)V9 COMP-3.
                    15 GSZUGBL           PIC S9(9)V9 COMP-3.
                    15 GSABGEL           PIC S9(9)V9 COMP-3.
                    15 GSABGBL           PIC S9(9)V9 COMP-3.
                    15 GSNETEL           PIC S9(9)V9 COMP-3.
                    15 GSNETBL           PIC S9(9)V9 COMP-3.
                    15 GSNETFEL          PIC S9(9)V9 COMP-3.
                    15 GSNETFBL          PIC S9(9)V9 COMP-3.
                    15 GSSTOEL           PIC S9(9)V9 COMP-3.
                    15 GSSTOBL           PIC S9(9)V9 COMP-3.
                    15 GSSTOGL           PIC S9(9)V9 COMP-3.
      *
      *** D D R - G E S C H Ä F T  (E I N H E I T E N)
      *
                 10 DDR-GESCHAEFT.
                    15 DDRZUGAE-EIG     PIC S9(9)V9 COMP-3.
                    15 DDRABGAE-EIG     PIC S9(9)V9 COMP-3.
                    15 DDRNETAE-EIG     PIC S9(9)V9 COMP-3.
                    15 DDRZUGAE-BET     PIC S9(9)V9 COMP-3.
                    15 DDRABGAE-BET     PIC S9(9)V9 COMP-3.
                    15 DDRNETAE-BET     PIC S9(9)V9 COMP-3.
                    15 DDRZUGAE-GES     PIC S9(9)V9 COMP-3.
                    15 DDRABGAE-GES     PIC S9(9)V9 COMP-3.
                    15 DDRNETAE-GES     PIC S9(9)V9 COMP-3.
                    15 DDRAE-PROZ       PIC S9(9)V99 COMP-3.
                    15 DDREQF-EIG       PIC S9(9)V99 COMP-3.
                    15 DDREQF-BET       PIC S9(9)V99 COMP-3.
                    15 DDREQF-GES       PIC S9(9)V99 COMP-3.
      *
      *** V M - A N T E I L  D D R  (E I N H E I T E N)
      *
                 10 DDR-VM-ANTEIL.
                    15 DDRZUGAE-VM-EIG  PIC S9(9)V9 COMP-3.
                    15 DDRABGAE-VM-EIG  PIC S9(9)V9 COMP-3.
                    15 DDRNETAE-VM-EIG  PIC S9(9)V9 COMP-3.
                    15 DDRZUGAE-VM-BET  PIC S9(9)V9 COMP-3.
                    15 DDRABGAE-VM-BET  PIC S9(9)V9 COMP-3.
                    15 DDRNETAE-VM-BET  PIC S9(9)V9 COMP-3.
                    15 DDRZUGAE-VM-GES  PIC S9(9)V9 COMP-3.
                    15 DDRABGAE-VM-GES  PIC S9(9)V9 COMP-3.
                    15 DDRNETAE-VM-GES  PIC S9(9)V9 COMP-3.
      *
      *** V M - A N T E I L  G E S A M T  (E I N H E I T E N)
      *
                 10 GES-VM-ANTEIL.
                    15 GESZUGAE-VM-EIG  PIC S9(9)V9 COMP-3.
                    15 GESABGAE-VM-EIG  PIC S9(9)V9 COMP-3.
                    15 GESNETAE-VM-EIG  PIC S9(9)V9 COMP-3.
                    15 GESZUGAE-VM-BET  PIC S9(9)V9 COMP-3.
                    15 GESABGAE-VM-BET  PIC S9(9)V9 COMP-3.
                    15 GESNETAE-VM-BET  PIC S9(9)V9 COMP-3.
                    15 GESZUGAE-VM-GES  PIC S9(9)V9 COMP-3.
                    15 GESABGAE-VM-GES  PIC S9(9)V9 COMP-3.
                    15 GESNETAE-VM-GES  PIC S9(9)V9 COMP-3.
      *
      *** L E B E N S V E R S I C H E R U N G
      *
                 10 LEBEN.
                    15 LEZUGEL           PIC S9(9)V9 COMP-3.
                    15 LEZUGBL           PIC S9(9)V9 COMP-3.
                    15 LEABGEL           PIC S9(9)V9 COMP-3.
                    15 LEABGBL           PIC S9(9)V9 COMP-3.
                    15 LENETEL           PIC S9(9)V9 COMP-3.
                    15 LENETBL           PIC S9(9)V9 COMP-3.
                    15 LESTOEL           PIC S9(9)V9 COMP-3.
                    15 LESTOBL           PIC S9(9)V9 COMP-3.
                    15 LESTOGL           PIC S9(9)V9 COMP-3.
      *
      *** D Y N A M I K
      *
                 10 DYNAMIKQ.
                    15 DYNQEL            PIC S9(9)V9 COMP-3.
                    15 DYNQBL            PIC S9(9)V9 COMP-3.
                    15 DYNQGL            PIC S9(9)V9 COMP-3.
      *
      *** S T O R N O  L V
      *
                 10 NE-STORNO-LV.
                    15 NESTEL            PIC S9(9)V9 COMP-3.
                    15 NESTBL            PIC S9(9)V9 COMP-3.
                    15 NESTGL            PIC S9(9)V9 COMP-3.
      *
      *** P R O D - G E S A M T
      *
                 10 PROD-GESAMT.
                    15 PEGSG-BIS         PIC S9(11)V9 COMP-3.
                    15 PEGSE-AB          PIC S9(11)V9 COMP-3.
                    15 PEGSB-AB          PIC S9(11)V9 COMP-3.
      *
      *** P R O V I S I O N S - V E R D I E N S T
      *
                 10 AM-LEBEN.
                    15 AMLEL             PIC S9(9)V99 COMP-3.
                    15 AMLBL             PIC S9(9)V99 COMP-3.
      *
                 10 DYNAMIKERH.
                    15 DYEEL             PIC S9(9)V99 COMP-3.
                    15 DYEBL             PIC S9(9)V99 COMP-3.
      *
                 10 AM-UNFALL.
                    15 AMUEL             PIC S9(9)V99 COMP-3.
                    15 AMUBL             PIC S9(9)V99 COMP-3.
                    15 AMUFEL            PIC S9(9)V99 COMP-3.
                    15 AMUFBL            PIC S9(9)V99 COMP-3.
      *
                 10 AM-H-H-G.
                    15 AMHHGEL           PIC S9(9)V99 COMP-3.
                    15 AMHHGBL           PIC S9(9)V99 COMP-3.
                    15 AMHHGFEL          PIC S9(9)V99 COMP-3.
                    15 AMHHGFBL          PIC S9(9)V99 COMP-3.
      *
                 10 AM-KFZ.
                    15 AMKEL             PIC S9(9)V99 COMP-3.
                    15 AMKBL             PIC S9(9)V99 COMP-3.
                    15 AMKFEL            PIC S9(9)V99 COMP-3.
                    15 AMKFBL            PIC S9(9)V99 COMP-3.
      *
                 10 AM-SONST.
                    15 AMSEL             PIC S9(9)V99 COMP-3.
                    15 AMSBL             PIC S9(9)V99 COMP-3.
                    15 AMSFEL            PIC S9(9)V99 COMP-3.
                    15 AMSFBL            PIC S9(9)V99 COMP-3.
      *
                 10 FOLGEPROV.
                    15 FOPEL             PIC S9(9)V99 COMP-3.
                    15 FOPBL             PIC S9(9)V99 COMP-3.
      *
                 10 BS.
                    15 BADEL             PIC S9(9)V99 COMP-3.
                    15 BADBL             PIC S9(9)V99 COMP-3.
      *
                 10 CKV.
                    15 CKVEL             PIC S9(9)V99 COMP-3.
                    15 CKVBL             PIC S9(9)V99 COMP-3.
      *
                 10 INVEST.
                    15 INVEL             PIC S9(9)V99 COMP-3.
                    15 INVBL             PIC S9(9)V99 COMP-3.
                    15 INVFEL            PIC S9(9)V99 COMP-3.
                    15 INVFBL            PIC S9(9)V99 COMP-3.
      *
                 10 IMMO.
                    15 IMMEL             PIC S9(9)V99 COMP-3.
                    15 IMMBL             PIC S9(9)V99 COMP-3.
                    15 IMMFEL            PIC S9(9)V99 COMP-3.
                    15 IMMFBL            PIC S9(9)V99 COMP-3.
      *
                 10 BANKEN.
                    15 BKEL              PIC S9(9)V99 COMP-3.
                    15 BKBL              PIC S9(9)V99 COMP-3.
                    15 BKFEL             PIC S9(9)V99 COMP-3.
                    15 BKFBL             PIC S9(9)V99 COMP-3.
      *
                 10 SONSTIGE.
                    15 SONEL             PIC S9(9)V99 COMP-3.
                    15 SONBL             PIC S9(9)V99 COMP-3.
                    15 SONFEL            PIC S9(9)V99 COMP-3.
                    15 SONFBL            PIC S9(9)V99 COMP-3.
      *
      *** P R O D - A R T L V
      *
                 10 PROD-ARTLV.
      *** K L V
                    15 KLV.
                       20 KLZUGEL        PIC S9(9)V9 COMP-3.
                       20 KLZUGBL        PIC S9(9)V9 COMP-3.
                       20 KLABGEL        PIC S9(9)V9 COMP-3.
                       20 KLABGBL        PIC S9(9)V9 COMP-3.
                       20 KLSTOEL        PIC S9(9)V9 COMP-3.
                       20 KLSTOBL        PIC S9(9)V9 COMP-3.
                       20 KLSTOGL        PIC S9(9)V9 COMP-3.
      *** F L V
                    15 FLV.
                       20 FLZUGEL        PIC S9(9)V9 COMP-3.
                       20 FLZUGBL        PIC S9(9)V9 COMP-3.
                       20 FLABGEL        PIC S9(9)V9 COMP-3.
                       20 FLABGBL        PIC S9(9)V9 COMP-3.
                       20 FLSTOEL        PIC S9(9)V9 COMP-3.
                       20 FLSTOBL        PIC S9(9)V9 COMP-3.
                       20 FLSTOGL        PIC S9(9)V9 COMP-3.
      *** R L V
                    15 RLV.
                       20 RLZUGEL        PIC S9(9)V9 COMP-3.
                       20 RLZUGBL        PIC S9(9)V9 COMP-3.
                       20 RLABGEL        PIC S9(9)V9 COMP-3.
                       20 RLABGBL        PIC S9(9)V9 COMP-3.
                       20 RLSTOEL        PIC S9(9)V9 COMP-3.
                       20 RLSTOBL        PIC S9(9)V9 COMP-3.
                       20 RLSTOGL        PIC S9(9)V9 COMP-3.
      *** V L V
                    15 VLV.
                       20 VLZUGEL        PIC S9(9)V9 COMP-3.
                       20 VLZUGBL        PIC S9(9)V9 COMP-3.
                       20 VLABGEL        PIC S9(9)V9 COMP-3.
                       20 VLABGBL        PIC S9(9)V9 COMP-3.
                       20 VLSTOEL        PIC S9(9)V9 COMP-3.
                       20 VLSTOBL        PIC S9(9)V9 COMP-3.
                       20 VLSTOGL        PIC S9(9)V9 COMP-3.
      *** B E R U F S U N F Ä H I G K E I T S V E R S I C H E R U N G
                    15 BU.
                       20 BUZUGEL        PIC S9(9)V9 COMP-3.
                       20 BUZUGBL        PIC S9(9)V9 COMP-3.
                       20 BUABGEL        PIC S9(9)V9 COMP-3.
                       20 BUABGBL        PIC S9(9)V9 COMP-3.
                       20 BUNETEL        PIC S9(9)V9 COMP-3.
                       20 BUNETBL        PIC S9(9)V9 COMP-3.
      *** S O N S T I G E  L E B E N
                    15 SONSTIGE-LEBEN.
                       20 SOZUGEL        PIC S9(9)V9 COMP-3.
                       20 SOZUGBL        PIC S9(9)V9 COMP-3.
                       20 SOABGEL        PIC S9(9)V9 COMP-3.
                       20 SOABGBL        PIC S9(9)V9 COMP-3.
                       20 SOSTOEL        PIC S9(9)V9 COMP-3.
                       20 SOSTOBL        PIC S9(9)V9 COMP-3.
                       20 SOSTOGL        PIC S9(9)V9 COMP-3.
      *
      *** P R O D - A R T N L V
      *
                 10 PROD-ARTNLV.
      *** U V
                    15 UV.
                       20 UVZUGEL        PIC S9(9)V9 COMP-3.
                       20 UVZUGBL        PIC S9(9)V9 COMP-3.
                       20 UVABGEL        PIC S9(9)V9 COMP-3.
                       20 UVABGBL        PIC S9(9)V9 COMP-3.
                       20 UVSTOEL        PIC S9(9)V9 COMP-3.
                       20 UVSTOBL        PIC S9(9)V9 COMP-3.
                       20 UVSTOGL        PIC S9(9)V9 COMP-3.
                       20 UVNETFEL       PIC S9(9)V9 COMP-3.
                       20 UVNETFBL       PIC S9(9)V9 COMP-3.
      *** K F Z
                    15 KFZ.
                       20 KFZUGEL        PIC S9(9)V9 COMP-3.
                       20 KFZUGBL        PIC S9(9)V9 COMP-3.
                       20 KFABGEL        PIC S9(9)V9 COMP-3.
                       20 KFABGBL        PIC S9(9)V9 COMP-3.
                       20 KFSTOEL        PIC S9(9)V9 COMP-3.
                       20 KFSTOBL        PIC S9(9)V9 COMP-3.
                       20 KFSTOGL        PIC S9(9)V9 COMP-3.
                       20 KFNETFEL       PIC S9(9)V9 COMP-3.
                       20 KFNETFBL       PIC S9(9)V9 COMP-3.
      *** H H G
                    15 H-H-G.
                       20 HHGZUGEL  PIC S9(9)V9 COMP-3.
                       20 HHGZUGBL  PIC S9(9)V9 COMP-3.
                       20 HHGABGEL  PIC S9(9)V9 COMP-3.
                       20 HHGABGBL  PIC S9(9)V9 COMP-3.
                       20 HHGSTOEL  PIC S9(9)V9 COMP-3.
                       20 HHGSTOBL  PIC S9(9)V9 COMP-3.
                       20 HHGSTOGL  PIC S9(9)V9 COMP-3.
                       20 HHGNETFEL PIC S9(9)V9 COMP-3.
                       20 HHGNETFBL PIC S9(9)V9 COMP-3.
      *** S V
                    15 SV.
                       20 SVZUGEL        PIC S9(9)V9 COMP-3.
                       20 SVZUGBL        PIC S9(9)V9 COMP-3.
                       20 SVABGEL        PIC S9(9)V9 COMP-3.
                       20 SVABGBL        PIC S9(9)V9 COMP-3.
                       20 SVSTOEL        PIC S9(9)V9 COMP-3.
                       20 SVSTOBL        PIC S9(9)V9 COMP-3.
                       20 SVSTOGL        PIC S9(9)V9 COMP-3.
                       20 SVNETFEL       PIC S9(9)V9 COMP-3.
                       20 SVNETFBL       PIC S9(9)V9 COMP-3.
      *** S A C H - F O L G E
                    15 SACH-FOLGE.
                       20 SZUGFEL        PIC S9(9)V9 COMP-3.
                       20 SZUGFBL        PIC S9(9)V9 COMP-3.
                       20 SABGFEL        PIC S9(9)V9 COMP-3.
                       20 SABGFBL        PIC S9(9)V9 COMP-3.
      *** R S
                    15 RS.
                       20 RSZUGEL        PIC S9(9)V9 COMP-3.
                       20 RSZUGBL        PIC S9(9)V9 COMP-3.
                       20 RSABGEL        PIC S9(9)V9 COMP-3.
                       20 RSABGBL        PIC S9(9)V9 COMP-3.
                       20 RSSTOEL        PIC S9(9)V9 COMP-3.
                       20 RSSTOBL        PIC S9(9)V9 COMP-3.
                       20 RSSTOGL        PIC S9(9)V9 COMP-3.
                       20 RSNETFEL       PIC S9(9)V9 COMP-3.
                       20 RSNETFBL       PIC S9(9)V9 COMP-3.
      *** B S
                    15 BS.
                       20 BSZUGEL        PIC S9(9)V9 COMP-3.
                       20 BSZUGBL        PIC S9(9)V9 COMP-3.
                       20 BSABGEL        PIC S9(9)V9 COMP-3.
                       20 BSABGBL        PIC S9(9)V9 COMP-3.
      *** K V
                    15 KV.
                       20 KVZUGEL        PIC S9(9)V9 COMP-3.
                       20 KVZUGBL        PIC S9(9)V9 COMP-3.
                       20 KVABGEL        PIC S9(9)V9 COMP-3.
                       20 KVABGBL        PIC S9(9)V9 COMP-3.
                       20 KVSTOEL        PIC S9(9)V9 COMP-3.
                       20 KVSTOBL        PIC S9(9)V9 COMP-3.
                       20 KVSTOGL        PIC S9(9)V9 COMP-3.
      *** I N V E S T
                    15 INVEST.
                       20 INZUGEL        PIC S9(9)V9 COMP-3.
                       20 INZUGBL        PIC S9(9)V9 COMP-3.
                       20 INABGEL        PIC S9(9)V9 COMP-3.
                       20 INABGBL        PIC S9(9)V9 COMP-3.
                       20 INSTOEL        PIC S9(9)V9 COMP-3.
                       20 INSTOBL        PIC S9(9)V9 COMP-3.
                       20 INSTOGL        PIC S9(9)V9 COMP-3.
                       20 INNETFEL       PIC S9(9)V9 COMP-3.
                       20 INNETFBL       PIC S9(9)V9 COMP-3.
      *** I M M O
                    15 IMMO.
                       20 IMZUGEL        PIC S9(9)V9 COMP-3.
                       20 IMZUGBL        PIC S9(9)V9 COMP-3.
                       20 IMABGEL        PIC S9(9)V9 COMP-3.
                       20 IMABGBL        PIC S9(9)V9 COMP-3.
                       20 IMSTOEL        PIC S9(9)V9 COMP-3.
                       20 IMSTOBL        PIC S9(9)V9 COMP-3.
                       20 IMSTOGL        PIC S9(9)V9 COMP-3.
                       20 IMNETFEL       PIC S9(9)V9 COMP-3.
                       20 IMNETFBL       PIC S9(9)V9 COMP-3.
      *** F O N D S - F O L G E
                    15 FONDS-FOLGE.
                       20 FOZUGFEL       PIC S9(9)V9 COMP-3.
                       20 FOZUGFBL       PIC S9(9)V9 COMP-3.
                       20 FOABGFEL       PIC S9(9)V9 COMP-3.
                       20 FOABGFBL       PIC S9(9)V9 COMP-3.
      *** B A N K E N
                    15 BANKEN.
                       20 BAZUGEL        PIC S9(9)V9 COMP-3.
                       20 BAZUGBL        PIC S9(9)V9 COMP-3.
                       20 BAABGEL        PIC S9(9)V9 COMP-3.
                       20 BAABGBL        PIC S9(9)V9 COMP-3.
                       20 BASTOEL        PIC S9(9)V9 COMP-3.
                       20 BASTOBL        PIC S9(9)V9 COMP-3.
                       20 BASTOGL        PIC S9(9)V9 COMP-3.
                       20 BANETFEL       PIC S9(9)V9 COMP-3.
                       20 BANETFBL       PIC S9(9)V9 COMP-3.
      *** S O N S T I G E
                    15 SONSTIGE.
                       20 STZUGEL        PIC S9(9)V9 COMP-3.
                       20 STZUGBL        PIC S9(9)V9 COMP-3.
                       20 STABGEL        PIC S9(9)V9 COMP-3.
                       20 STABGBL        PIC S9(9)V9 COMP-3.
                       20 STSTOEL        PIC S9(9)V9 COMP-3.
                       20 STSTOBL        PIC S9(9)V9 COMP-3.
                       20 STSTOGL        PIC S9(9)V9 COMP-3.
                       20 STNETFEL       PIC S9(9)V9 COMP-3.
                       20 STNETFBL       PIC S9(9)V9 COMP-3.
      *
      *** O E S T E R R  -  G E S E L L S C H A F T
      *
                 10 OESTERR-GESELL.
      *** W A L - P R O V E I N H
                    15 WAL-PROVEINH.
                       20 WALZUGEL       PIC S9(11)V99 COMP-3.
                       20 WALZUGBL       PIC S9(11)V99 COMP-3.
                       20 WALABGEL       PIC S9(11)V99 COMP-3.
                       20 WALABGBL       PIC S9(11)V99 COMP-3.
                       20 WALNETEL       PIC S9(11)V99 COMP-3.
                       20 WALNETBL       PIC S9(11)V99 COMP-3.
                       20 WALSTOEL       PIC S9(05)V99 COMP-3.
                       20 WALSTOBL       PIC S9(05)V99 COMP-3.
                       20 WALSTOGL       PIC S9(05)V99 COMP-3.
      *** W A L - N E - S T O R N O - L V
                    15 WAL-NE-STORNO-LV.
                       20 WALNESTEL      PIC S9(05)V99 COMP-3.
                       20 WALNESTBL      PIC S9(05)V99 COMP-3.
                       20 WALNESTGL      PIC S9(05)V99 COMP-3.
      *** W A L - P R O V V E R D
                    15 WAL-PROVVERD.
                       20 WALEL          PIC S9(11)V99 COMP-3.
                       20 WALBL          PIC S9(11)V99 COMP-3.
      *** W A L - D Y N M I K E R H
                    15 WAL-DYNAMIKERH.
                       20 WALDYEEL       PIC S9(11)V99 COMP-3.
                       20 WALDYEBL       PIC S9(11)V99 COMP-3.
      *** W A V - U N F A L L
                    15 WAV-UNFALL.
                       20 WAVUEL         PIC S9(11)V99 COMP-3.
                       20 WAVUBL         PIC S9(11)V99 COMP-3.
                       20 WAVUFEL        PIC S9(11)V99 COMP-3.
                       20 WAVUFBL        PIC S9(11)V99 COMP-3.
      *** W A V - H H G
                    15 WAV-H-H-G.
                       20 WAVHHGEL       PIC S9(11)V99 COMP-3.
                       20 WAVHHGBL       PIC S9(11)V99 COMP-3.
                       20 WAVHHGFEL      PIC S9(11)V99 COMP-3.
                       20 WAVHHGFBL      PIC S9(11)V99 COMP-3.
      *** W A V - R S
                    15 WAV-RS.
                       20 WAVRSEL        PIC S9(11)V99 COMP-3.
                       20 WAVRSBL        PIC S9(11)V99 COMP-3.
                       20 WAVRSFEL       PIC S9(11)V99 COMP-3.
                       20 WAVRSFBL       PIC S9(11)V99 COMP-3.
      *** W A V - V S
                    15 WAV-VS.
                       20 WAVVSEL        PIC S9(11)V99 COMP-3.
                       20 WAVVSBL        PIC S9(11)V99 COMP-3.
                       20 WAVVSFEL       PIC S9(11)V99 COMP-3.
                       20 WAVVSFBL       PIC S9(11)V99 COMP-3.
      *** W A V - K V
                    15 WAV-KV.
                       20 WAVKVEL        PIC S9(11)V99 COMP-3.
                       20 WAVKVBL        PIC S9(11)V99 COMP-3.
                       20 WAVKVFEL       PIC S9(11)V99 COMP-3.
                       20 WAVKVFBL       PIC S9(11)V99 COMP-3.
      *** W A  - F O L G E P R O V
                    15 WA-FOLGEPROV.
                       20 WAFOPEL        PIC S9(11)V99 COMP-3.
                       20 WAFOPBL        PIC S9(11)V99 COMP-3.
      *** O E G - B S K
                    15 OEG-BSK.
                       20 OEGBSKEL       PIC S9(11)V99 COMP-3.
                       20 OEGBSKBL       PIC S9(11)V99 COMP-3.
                       20 OEGBSKFEL      PIC S9(11)V99 COMP-3.
                       20 OEGBSKFBL      PIC S9(11)V99 COMP-3.
      *** O E G - I N V E S T
                    15 OEG-INVEST.
                       20 OEGINVEL       PIC S9(11)V99 COMP-3.
                       20 OEGINVBL       PIC S9(11)V99 COMP-3.
                       20 OEGINVFEL      PIC S9(11)V99 COMP-3.
                       20 OEGINVFBL      PIC S9(11)V99 COMP-3.
      *** O E G - I M M O
                    15 OEG-IMMO.
                       20 OEGIMMEL       PIC S9(11)V99 COMP-3.
                       20 OEGIMMBL       PIC S9(11)V99 COMP-3.
                       20 OEGIMMFEL      PIC S9(11)V99 COMP-3.
                       20 OEGIMMFBL      PIC S9(11)V99 COMP-3.
      *** O E G - B A N K E N
                    15 OEG-BANKEN.
                       20 OEGBKEL        PIC S9(11)V99 COMP-3.
                       20 OEGBKBL        PIC S9(11)V99 COMP-3.
                       20 OEGBKFEL       PIC S9(11)V99 COMP-3.
                       20 OEGBKFBL       PIC S9(11)V99 COMP-3.
      *** O E G - P R O D - A R T L V
                    15 OEG-PROD-ARTLV.
                       20 OEG-KLV.
                          25 OEGKLZUGEL  PIC S9(11)V99 COMP-3.
                          25 OEGKLZUGBL  PIC S9(11)V99 COMP-3.
                          25 OEGKLABGEL  PIC S9(11)V99 COMP-3.
                          25 OEGKLABGBL  PIC S9(11)V99 COMP-3.
                          25 OEGKLSTOEL  PIC S9(11)V99 COMP-3.
                          25 OEGKLSTOBL  PIC S9(11)V99 COMP-3.
                          25 OEGKLSTOGL  PIC S9(11)V99 COMP-3.
                       20 OEG-RLV.
                          25 OEGRLZUGEL  PIC S9(11)V99 COMP-3.
                          25 OEGRLZUGBL  PIC S9(11)V99 COMP-3.
                          25 OEGRLABGEL  PIC S9(11)V99 COMP-3.
                          25 OEGRLABGBL  PIC S9(11)V99 COMP-3.
                          25 OEGRLSTOEL  PIC S9(11)V99 COMP-3.
                          25 OEGRLSTOBL  PIC S9(11)V99 COMP-3.
                          25 OEGRLSTOGL  PIC S9(11)V99 COMP-3.
                       20 OEG-BUZ.
                          25 OEGBUZUGEL  PIC S9(11)V99 COMP-3.
                          25 OEGBUZUGBL  PIC S9(11)V99 COMP-3.
                          25 OEGBUABGEL  PIC S9(11)V99 COMP-3.
                          25 OEGBUABGBL  PIC S9(11)V99 COMP-3.
                          25 OEGBUSTOEL  PIC S9(11)V99 COMP-3.
                          25 OEGBUSTOBL  PIC S9(11)V99 COMP-3.
                          25 OEGBUSTOGL  PIC S9(11)V99 COMP-3.
                       20 OEG-UZV.
                          25 OEGUZZUGEL  PIC S9(11)V99 COMP-3.
                          25 OEGUZZUGBL  PIC S9(11)V99 COMP-3.
                          25 OEGUZABGEL  PIC S9(11)V99 COMP-3.
                          25 OEGUZABGBL  PIC S9(11)V99 COMP-3.
                          25 OEGUZSTOEL  PIC S9(11)V99 COMP-3.
                          25 OEGUZSTOBL  PIC S9(11)V99 COMP-3.
                          25 OEGUZSTOGL  PIC S9(11)V99 COMP-3.
      *
      *** O E G - P R O D - A R T N L V
      *
                    15 OEG-PROD-ARTNLV.
      *** O E G - U V
                       20 OEG-UV.
                          25 OEGUVZUGEL   PIC S9(11)V99 COMP-3.
                          25 OEGUVZUGBL   PIC S9(11)V99 COMP-3.
                          25 OEGUVABGEL   PIC S9(11)V99 COMP-3.
                          25 OEGUVABGBL   PIC S9(11)V99 COMP-3.
                          25 OEGUVSTOEL   PIC S9(11)V99 COMP-3.
                          25 OEGUVSTOBL   PIC S9(11)V99 COMP-3.
                          25 OEGUVSTOGL   PIC S9(11)V99 COMP-3.
                          25 OEGUVNETFEL  PIC S9(11)V99 COMP-3.
                          25 OEGUVNETFBL  PIC S9(11)V99 COMP-3.
      *** O E G - H - H - G
                       20 OEG-H-H-G.
                          25 OEGHHGZUGEL  PIC S9(11)V99 COMP-3.
                          25 OEGHHGZUGBL  PIC S9(11)V99 COMP-3.
                          25 OEGHHGABGEL  PIC S9(11)V99 COMP-3.
                          25 OEGHHGABGBL  PIC S9(11)V99 COMP-3.
                          25 OEGHHGSTOEL  PIC S9(11)V99 COMP-3.
                          25 OEGHHGSTOBL  PIC S9(11)V99 COMP-3.
                          25 OEGHHGSTOGL  PIC S9(11)V99 COMP-3.
                          25 OEGHHGNETFEL  PIC S9(11)V99 COMP-3.
                          25 OEGHHGNETFBL  PIC S9(11)V99 COMP-3.
      *** O E G - R - S
                       20 OEG-RS.
                          25 OEGRSZUGEL  PIC S9(11)V99 COMP-3.
                          25 OEGRSZUGBL  PIC S9(11)V99 COMP-3.
                          25 OEGRSABGEL  PIC S9(11)V99 COMP-3.
                          25 OEGRSABGBL  PIC S9(11)V99 COMP-3.
                          25 OEGRSSTOEL  PIC S9(11)V99 COMP-3.
                          25 OEGRSSTOBL  PIC S9(11)V99 COMP-3.
                          25 OEGRSSTOGL  PIC S9(11)V99 COMP-3.
                          25 OEGRSNETFEL  PIC S9(11)V99 COMP-3.
                          25 OEGRSNETFBL  PIC S9(11)V99 COMP-3.
      *** O E G - V - S
                       20 OEG-VS.
                          25 OEGVSZUGEL  PIC S9(11)V99 COMP-3.
                          25 OEGVSZUGBL  PIC S9(11)V99 COMP-3.
                          25 OEGVSABGEL  PIC S9(11)V99 COMP-3.
                          25 OEGVSABGBL  PIC S9(11)V99 COMP-3.
                          25 OEGVSSTOEL  PIC S9(11)V99 COMP-3.
                          25 OEGVSSTOBL  PIC S9(11)V99 COMP-3.
                          25 OEGVSSTOGL  PIC S9(11)V99 COMP-3.
                          25 OEGVSNETFEL  PIC S9(11)V99 COMP-3.
                          25 OEGVSNETFBL  PIC S9(11)V99 COMP-3.
      *** O E G - K - V
                       20 OEG-KV.
                          25 OEGKVZUGEL  PIC S9(11)V99 COMP-3.
                          25 OEGKVZUGBL  PIC S9(11)V99 COMP-3.
                          25 OEGKVABGEL  PIC S9(11)V99 COMP-3.
                          25 OEGKVABGBL  PIC S9(11)V99 COMP-3.
                          25 OEGKVSTOEL  PIC S9(11)V99 COMP-3.
                          25 OEGKVSTOBL  PIC S9(11)V99 COMP-3.
                          25 OEGKVSTOGL  PIC S9(11)V99 COMP-3.
                          25 OEGKVNETFEL  PIC S9(11)V99 COMP-3.
                          25 OEGKVNETFBL  PIC S9(11)V99 COMP-3.
      *** O E G - S A C H - F O L G E
                       20 OEG-SACH-FOLGE.
                          25 OEGSZUGFEL  PIC S9(11)V99 COMP-3.
                          25 OEGSZUGFBL  PIC S9(11)V99 COMP-3.
                          25 OEGSABGFEL  PIC S9(11)V99 COMP-3.
                          25 OEGSABGFBL  PIC S9(11)V99 COMP-3.
      *** O E G - B S K
                       20 OEG-BSK.
                          25 OEGBSKZUGEL  PIC S9(11)V99 COMP-3.
                          25 OEGBSKZUGBL  PIC S9(11)V99 COMP-3.
                          25 OEGBSKABGEL  PIC S9(11)V99 COMP-3.
                          25 OEGBSKABGBL  PIC S9(11)V99 COMP-3.
                          25 OEGBSKSTOEL  PIC S9(11)V99 COMP-3.
                          25 OEGBSKSTOBL  PIC S9(11)V99 COMP-3.
                          25 OEGBSKSTOGL  PIC S9(11)V99 COMP-3.
                          25 OEGBSKNETFEL  PIC S9(11)V99 COMP-3.
                          25 OEGBSKNETFBL  PIC S9(11)V99 COMP-3.
      *** O E G - I N V E S T
                       20 OEG-INVEST.
                          25 OEGINZUGEL  PIC S9(11)V99 COMP-3.
                          25 OEGINZUGBL  PIC S9(11)V99 COMP-3.
                          25 OEGINABGEL  PIC S9(11)V99 COMP-3.
                          25 OEGINABGBL  PIC S9(11)V99 COMP-3.
                          25 OEGINSTOEL  PIC S9(11)V99 COMP-3.
                          25 OEGINSTOBL  PIC S9(11)V99 COMP-3.
                          25 OEGINSTOGL  PIC S9(11)V99 COMP-3.
                          25 OEGINNETFEL  PIC S9(11)V99 COMP-3.
                          25 OEGINNETFBL  PIC S9(11)V99 COMP-3.
      *** O E G - I M M O
                       20 OEG-IMMO.
                          25 OEGIMZUGEL  PIC S9(11)V99 COMP-3.
                          25 OEGIMZUGBL  PIC S9(11)V99 COMP-3.
                          25 OEGIMABGEL  PIC S9(11)V99 COMP-3.
                          25 OEGIMABGBL  PIC S9(11)V99 COMP-3.
                          25 OEGIMSTOEL  PIC S9(11)V99 COMP-3.
                          25 OEGIMSTOBL  PIC S9(11)V99 COMP-3.
                          25 OEGIMSTOGL  PIC S9(11)V99 COMP-3.
                          25 OEGIMNETFEL  PIC S9(11)V99 COMP-3.
                          25 OEGIMNETFBL  PIC S9(11)V99 COMP-3.
      *** O E G - F O N D S - F O L G E
                       20 OEG-FONDS-FOLGE.
                          25 OEGFOZUGFEL  PIC S9(11)V99 COMP-3.
                          25 OEGFOZUGFBL  PIC S9(11)V99 COMP-3.
                          25 OEGFOABGFEL  PIC S9(11)V99 COMP-3.
                          25 OEGFOABGFBL  PIC S9(11)V99 COMP-3.
      *** O E G - B A N K E N
                       20 OEG-BANKEN.
                          25 OEGBAZUGEL  PIC S9(11)V99 COMP-3.
                          25 OEGBAZUGBL  PIC S9(11)V99 COMP-3.
                          25 OEGBAABGEL  PIC S9(11)V99 COMP-3.
                          25 OEGBAABGBL  PIC S9(11)V99 COMP-3.
                          25 OEGBASTOEL  PIC S9(11)V99 COMP-3.
                          25 OEGBASTOBL  PIC S9(11)V99 COMP-3.
                          25 OEGBASTOGL  PIC S9(11)V99 COMP-3.
                          25 OEGBANETFEL  PIC S9(11)V99 COMP-3.
                          25 OEGBANETFBL  PIC S9(11)V99 COMP-3.
      *** REST - FILLER
              05 FILLER                  PIC X(57).
      *
      *** REDEFINITIONE FÜR DUMMY-SATZ
      *
           03 VSATZ-DUM REDEFINES VSKEY-REST-DATEN.
              05 VSDAT-DUM               PIC 9(4).
              05 VSREST-DUM              PIC X(2784).
