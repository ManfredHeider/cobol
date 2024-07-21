      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0035.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRAN0035.CPv  $
      *
      *   Rev 2.11   19 May 2006 15:06:20   t13ae23
      *AAN00666: Erweiterung der Produktpalette um weitere Rürup-Ta
      *rife
      *
      *   Rev 2.10   10 May 2006 17:02:54   T13AE23
      *AAN00666: Erweiterung der Produktpalette um weitere Rürup-Ta
      *rife
      *
      *   Rev 2.9   Nov 14 2005 15:09:10   t13ae34
      *AAN00626: Einführung des Tarifes FRBZ
      *
      *   Rev 2.8   May 23 2005 14:10:26   t13ae34
      *AAN00583: Anpassung AML DFÜ-SST zur Auswahl 4. Fonds 
      *
      *   Rev 2.7   Oct 28 2004 14:27:50   t13ae34
      *AAN00531: Neue Tariflandschaft AML 2005 im eingereichten Ges
      *chäft 
      *
      *   Rev 2.6   Jun 01 2004 11:46:06   t13ae34
      *Neu Selbst. Kinderinvaliditätsversicherung und Kinderin-
      *
      *   Rev 2.5   Nov 19 2003 12:59:14   t13ae34
      *AAN00471 - Änderung SST PZM AML
      *
      *   Rev 2.4   Feb 17 2003 16:56:42   t13ae34
      *AAN00426 - Anpassung AML PZM 2.Produktpaket Risiko
      *
      *   Rev 2.3   Nov 09 2001 16:03:58   t13ae34
      *AAN00356 - PZM-DFÜ Zulagenvertrag+Nettoverfahren
      *
      *   Rev 2.2   30 Apr 2001 09:09:28   T13AE34
      *FAN00302-ZTAN0002 mit Fkt. Insert
      *
      *   Rev 2.1   07 Mar 2001 09:56:02   T13AE34
      *AAN00268-PZM Antrag
      *
      *   Rev 2.0   Feb 27 1999 11:05:36   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:36:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0035  Funktion Insert Standard-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00268!23.11.00!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * FAN00302!27.04.01!2.2 ! FKT. INSERT               ! HEIDER   *
      * AAN00356!08.11.01!2.3 ! ZULAGEN+NETTOVERFAHREN    ! HEIDER   *
      * AAN00426!31.01.03!2.4 ! 2.PRODUKTPAKET AML        ! HEIDER   *
      * AAN00471!19.11.03!2.5 ! ÄNDERUNG SST PZM AML      ! HEIDER   *
      * AAN00502!28.05.04!2.6 ! ÄNDERUNG SST KINDERINVAL  ! HEIDER   *
      * AAN00531!22.10.04!2.7 !AML NEUE TARIFE NACH AEG   !HEIDER    *
      * AAN00583!23.05.05!2.8 !AML 4 FONDS                !HEIDER    *
      * AAN00666!10.05.06!2.9 !NEUE RÜRUP-TARIFE          !RAHNER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0035                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PZMA TBAN0002              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0035.
           05 PI-DRAN0035-PZMA-KEY.
               10 PI-DRAN0035-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-HVANTNUM    PIC X(9).
           05 PI-DRAN0035-PZMA-DATEN.
               10 PI-DRAN0035-PZM-PIN         PIC S9(9) COMP.
               10 PI-DRAN0035-PZM-EVNVT1      PIC X.
               10 PI-DRAN0035-PZM-EVNVT2      PIC X.
               10 PI-DRAN0035-PZM-PDVNANRED   PIC X(2).
               10 PI-DRAN0035-PZM-PDVNTITEL   PIC X(14).
               10 PI-DRAN0035-PZM-PDVNVORNA   PIC X(16).
               10 PI-DRAN0035-PZM-PDVNZUNA    PIC X(33).
               10 PI-DRAN0035-PZM-PDVNZUS     PIC X(15).
               10 PI-DRAN0035-PZM-PDVNGBNA    PIC X(21).
               10 PI-DRAN0035-PZM-PDVNSTR     PIC X(25).
               10 PI-DRAN0035-PZM-PDVNLKZ     PIC X(3).
               10 PI-DRAN0035-PZM-PDVNLAND    PIC X(35).
               10 PI-DRAN0035-PZM-PDVNPLZ     PIC X(6).
               10 PI-DRAN0035-PZM-PDVNORT     PIC X(30).
               10 PI-DRAN0035-PZM-PDVNTELPR   PIC X(23).
               10 PI-DRAN0035-PZM-PDVNFAX     PIC X(23).
               10 PI-DRAN0035-PZM-PDVNTELDI   PIC X(23).
               10 PI-DRAN0035-PZM-PDVNGBDAT   PIC X(10).
               10 PI-DRAN0035-PZM-PDVNGBORT   PIC X(30).
               10 PI-DRAN0035-PZM-PDVNFAM     PIC X.
               10 PI-DRAN0035-PZM-PDVNSELBST  PIC X.
               10 PI-DRAN0035-PZM-PDVNBER     PIC X(40).
               10 PI-DRAN0035-PZM-PDVNBERKY   PIC X(6).
               10 PI-DRAN0035-PZM-PDVNSTAAT   PIC X(35).
               10 PI-DRAN0035-PZM-PDVNSTAATKZ PIC X(03).
               10 PI-DRAN0035-PZM-PDV1ANRED   PIC X(2).
               10 PI-DRAN0035-PZM-PDV1TITEL   PIC X(14).
               10 PI-DRAN0035-PZM-PDV1VORNA   PIC X(16).
               10 PI-DRAN0035-PZM-PDV1ZUNA    PIC X(21).
               10 PI-DRAN0035-PZM-PDV1ZUS     PIC X(15).
               10 PI-DRAN0035-PZM-PDV1GBNA    PIC X(21).
               10 PI-DRAN0035-PZM-PDV1STR     PIC X(25).
               10 PI-DRAN0035-PZM-PDV1LKZ     PIC X(3).
               10 PI-DRAN0035-PZM-PDV1LAND    PIC X(35).
               10 PI-DRAN0035-PZM-PDV1PLZ     PIC X(6).
               10 PI-DRAN0035-PZM-PDV1ORT     PIC X(30).
               10 PI-DRAN0035-PZM-PDV1GBDAT   PIC X(10).
               10 PI-DRAN0035-PZM-PDV1GBORT   PIC X(30).
               10 PI-DRAN0035-PZM-PDV1FAM     PIC X.
               10 PI-DRAN0035-PZM-PDV1SELBST  PIC X.
               10 PI-DRAN0035-PZM-PDV1BER     PIC X(40).
               10 PI-DRAN0035-PZM-PDV1BERKY   PIC X(6).
               10 PI-DRAN0035-PZM-PDV1STAAT   PIC X(35).
               10 PI-DRAN0035-PZM-PDV1STAATKZ PIC X(03).
               10 PI-DRAN0035-PZM-PDV2ANRED   PIC X(2).
               10 PI-DRAN0035-PZM-PDV2TITEL   PIC X(14).
               10 PI-DRAN0035-PZM-PDV2VORNA   PIC X(16).
               10 PI-DRAN0035-PZM-PDV2ZUNA    PIC X(21).
               10 PI-DRAN0035-PZM-PDV2ZUS     PIC X(15).
               10 PI-DRAN0035-PZM-PDV2GBNA    PIC X(21).
               10 PI-DRAN0035-PZM-PDV2STR     PIC X(25).
               10 PI-DRAN0035-PZM-PDV2LKZ     PIC X(3).
               10 PI-DRAN0035-PZM-PDV2LAND    PIC X(35).
               10 PI-DRAN0035-PZM-PDV2PLZ     PIC X(6).
               10 PI-DRAN0035-PZM-PDV2ORT     PIC X(30).
               10 PI-DRAN0035-PZM-PDV2GBDAT   PIC X(10).
               10 PI-DRAN0035-PZM-PDV2GBORT   PIC X(30).
               10 PI-DRAN0035-PZM-PDV2FAM     PIC X.
               10 PI-DRAN0035-PZM-PDV2SELBST  PIC X.
               10 PI-DRAN0035-PZM-PDV2BER     PIC X(40).
               10 PI-DRAN0035-PZM-PDV2BERKY   PIC X(6).
               10 PI-DRAN0035-PZM-PDV2STAAT   PIC X(35).
               10 PI-DRAN0035-PZM-PDV2STAATKZ PIC X(03).
               10 PI-DRAN0035-PZM-VDSPARTE    PIC X.
               10 PI-DRAN0035-PZM-VDGKZ       PIC X(2).
               10 PI-DRAN0035-PZM-VDABVERM    PIC S9(9) COMP.
               10 PI-DRAN0035-PZM-VDABVERMPZ  PIC X.
               10 PI-DRAN0035-PZM-VDVMVORNA   PIC X(16).
               10 PI-DRAN0035-PZM-VDVMZUNA    PIC X(21).
               10 PI-DRAN0035-PZM-VDVMNR      PIC S9(9) COMP.
               10 PI-DRAN0035-PZM-VDVMNRPZ    PIC X.
               10 PI-DRAN0035-PZM-VDVMNRB     PIC X.
               10 PI-DRAN0035-PZM-VDPRODKEY   PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-VDABLEIN    PIC S9(6)V9(1) COMP-3.
191103*        10 PI-DRAN0035-PZM-VDBETREU    PIC X(7).
191103         10 PI-DRAN0035-PZM-VDBETREU    PIC X(9).
               10 PI-DRAN0035-PZM-VDDIREKT    PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-VDPIF       PIC X(15).
               10 PI-DRAN0035-PZM-VDPROGKZ    PIC X(3).
191103*        10 PI-DRAN0035-PZM-VERSION     PIC X(4).
191103         10 PI-DRAN0035-PZM-VERSION     PIC X(5).
               10 PI-DRAN0035-PZM-HVTARIF     PIC X(15).
               10 PI-DRAN0035-PZM-HVVSNR      PIC S9(9) COMP.
               10 PI-DRAN0035-PZM-HVVBEGINN   PIC X(10).
               10 PI-DRAN0035-PZM-HVN         PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-HVNS        PIC S9(4) COMP.
270401         10 PI-DRAN0035-PZM-HV8ENDALT   PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-HVBVNL      PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-EZAHLW      PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-HVBEITRAG   PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-HVRIS       PIC S9(6)V9(4) COMP-3.
               10 PI-DRAN0035-PZM-ZUEB        PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-HVVSUMME    PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-HVTAZGAR    PIC S9(9) COMP.
               10 PI-DRAN0035-PZM-TAJADF-1    PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-TAJADF-2    PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-TAJADF-3    PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-TAJADF-4    PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-TAJADF-5    PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-TAJADF-6    PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-TAJADF-7    PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-TAJADF-8    PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-TAJADF-9    PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-DM1FONDS    PIC X(2).
               10 PI-DRAN0035-PZM-DM1FONDSPROZ PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-DM2FONDS     PIC X(2).
               10 PI-DRAN0035-PZM-DM2FONDSPROZ PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-DM3FONDS     PIC X(2).
               10 PI-DRAN0035-PZM-DM3FONDSPROZ PIC S9(4) COMP.
230505*>>>>>
               10 PI-DRAN0035-PZM-DM4FONDS     PIC X(2).
               10 PI-DRAN0035-PZM-DM4FONDSPROZ PIC S9(4) COMP.
230505*<<<<<
               10 PI-DRAN0035-PZM-HVBEGREN    PIC X(10).
               10 PI-DRAN0035-PZM-HVREZAHLW   PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-HVGARZEIT   PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-HVRENTE     PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-HVRENTH2    PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-HVRENTH3    PIC S9(9)V9(2) COMP-3.
081101         10 PI-DRAN0035-PZM-HVRENTH4    PIC S9(3)      COMP-3.
081101         10 PI-DRAN0035-PZM-HVRENTH5    PIC S9(3)      COMP-3.
               10 PI-DRAN0035-PZM-HVKAPAB     PIC S9(9) COMP.
               10 PI-DRAN0035-PZM-HZVRENT1    PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-HZVP        PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-HVRENTDMN   PIC S9(9)V9(2) COMP-3.
270401         10 PI-DRAN0035-PZM-AVMG        PIC X.
270401         10 PI-DRAN0035-PZM-BBG         PIC X.
081101         10 PI-DRAN0035-PZM-NVERF       PIC X.
081101         10 PI-DRAN0035-PZM-ZBE         PIC X.
               10 PI-DRAN0035-PZM-BUZKZ       PIC X.
               10 PI-DRAN0035-PZM-BERBUEA     PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-BUBARREN1   PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-BUBN        PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-BUBZ        PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-BUBT        PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-BUBLI       PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-BUBLR       PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-BUZI        PIC X.
               10 PI-DRAN0035-PZM-BUZP        PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-BUZR        PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-BUZD        PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-BUZRIS      PIC S9(3)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-BUZBG       PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-DYDYNPROZ   PIC S9(2)V9(1) COMP-3.
               10 PI-DRAN0035-PZM-UZVKZ       PIC X.
               10 PI-DRAN0035-PZM-UZVP        PIC S9(3)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-UZVS        PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-UZVRIS      PIC S9(3)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-EUN         PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-EUT         PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-EUZS        PIC X.
               10 PI-DRAN0035-PZM-EUZRIS      PIC S9(3)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-EUZBG       PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-DEPOT       PIC X.
               10 PI-DRAN0035-PZM-SODEPHOE    PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-SODEPINS    PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-GEWVA       PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-ASAMNR      PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-EKAPWR      PIC X.
               10 PI-DRAN0035-PZM-ENACHV      PIC X.
               10 PI-DRAN0035-PZM-EUMTAU      PIC X.
               10 PI-DRAN0035-PZM-EWIEDERANL  PIC X.
               10 PI-DRAN0035-PZM-BESV        PIC X.
               10 PI-DRAN0035-PZM-GESKZ       PIC X.
               10 PI-DRAN0035-PZM-EUROKZ      PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-LAST-UPDATE PIC X(26).
               10 PI-DRAN0035-PZM-KENNUNG     PIC X(8).
310103*>>>>>
               10 PI-DRAN0035-PZM-PDVNGLKZ    PIC X(03).
               10 PI-DRAN0035-PZM-PDVNGLAND   PIC X(35).
               10 PI-DRAN0035-PZM-UEBART      PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-BUZUEBART   PIC S9(4) COMP.
310103*<<<<<
280504*>>>>>
               10 PI-DRAN0035-PZM-EVNVT3      PIC X.
               10 PI-DRAN0035-PZM-PDV3ANRED   PIC X(2).
               10 PI-DRAN0035-PZM-PDV3TITEL   PIC X(14).
               10 PI-DRAN0035-PZM-PDV3VORNA   PIC X(16).
               10 PI-DRAN0035-PZM-PDV3ZUNA    PIC X(21).
               10 PI-DRAN0035-PZM-PDV3ZUS     PIC X(15).
               10 PI-DRAN0035-PZM-PDV3GBNA    PIC X(21).
               10 PI-DRAN0035-PZM-PDV3STR     PIC X(25).
               10 PI-DRAN0035-PZM-PDV3LKZ     PIC X(3).
               10 PI-DRAN0035-PZM-PDV3LAND    PIC X(35).
               10 PI-DRAN0035-PZM-PDV3PLZ     PIC X(6).
               10 PI-DRAN0035-PZM-PDV3ORT     PIC X(30).
               10 PI-DRAN0035-PZM-PDV3GBDAT   PIC X(10).
               10 PI-DRAN0035-PZM-PDV3GBORT   PIC X(30).
               10 PI-DRAN0035-PZM-PDV3FAM     PIC X.
               10 PI-DRAN0035-PZM-PDV3SELBST  PIC X.
               10 PI-DRAN0035-PZM-PDV3BER     PIC X(40).
               10 PI-DRAN0035-PZM-PDV3BERKY   PIC X(6).
               10 PI-DRAN0035-PZM-PDV3STAAT   PIC X(35).
               10 PI-DRAN0035-PZM-PDV3STAATKZ PIC X(03).
               10 PI-DRAN0035-PZM-HVENDNS     PIC X(10).
               10 PI-DRAN0035-PZM-BUZABLGEMBZHV
                                              PIC X(01).
               10 PI-DRAN0035-PZM-KIZKZ       PIC X.
               10 PI-DRAN0035-PZM-KIZD        PIC S9(2) COMP-3.
               10 PI-DRAN0035-PZM-KIZR        PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-FIRMEINTRDATVT
                                              PIC X(10).
280504*<<<<<
221004*>>>>>
               10  PI-DRAN0035-PZM-DYDYND2    PIC X.
               10 PI-DRAN0035-PZM-TZVKZ       PIC X.
               10 PI-DRAN0035-PZM-TZVD        PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-TZVS        PIC S9(9)V9(2) COMP-3.

      *        HINTERBLIEBENEN-ZUSATZ
               10 PI-DRAN0035-PZM-HZVKZ       PIC X.
               10 PI-DRAN0035-PZM-HZVR        PIC S9(9)V9(2) COMP-3.

      *        STATISTIKDATEN DVAG
               10 PI-DRAN0035-PZM-DIVE           PIC X.
               10 PI-DRAN0035-PZM-VDPRODKEYBUZI  PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-VDABLEINBUZI   PIC S9(6)V9(1) COMP-3.
               10 PI-DRAN0035-PZM-BEITRAGBUZI    PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-VDPRODKEYBUZR  PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-VDABLEINBUZR   PIC S9(6)V9(1) COMP-3.
               10 PI-DRAN0035-PZM-BEITRAGBUZR    PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-VDPRODKEYKIZ   PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-VDABLEINKIZ    PIC S9(6)V9(1) COMP-3.
               10 PI-DRAN0035-PZM-BEITRAGKIZ     PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-VDPRODKEYTZV   PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-VDABLEINTZV    PIC S9(6)V9(1) COMP-3.
               10 PI-DRAN0035-PZM-BEITRAGTZV     PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-VDPRODKEYHZV   PIC S9(4) COMP.
               10 PI-DRAN0035-PZM-VDABLEINHZV    PIC S9(6)V9(1) COMP-3.
               10 PI-DRAN0035-PZM-BEITRAGHZV     PIC S9(9)V9(2) COMP-3.

221004*<<<<<
201005*>>>>>
               10 PI-DRAN0035-PZM-VAREU-5PRZ1    PIC S9(3)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-VAREU-5PRZ2    PIC S9(3)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-VAREU-5PRZ3    PIC S9(3)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-VAREU-5PRZ4    PIC S9(3)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-VAREU-5PRZ5    PIC S9(3)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-VAREU-5PRZ6    PIC S9(3)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-VAREU-5PRZ7    PIC S9(3)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-VAREU-5PRZ8    PIC S9(3)V9(2) COMP-3.
               10 PI-DRAN0035-PZM-HVGARZEITMAX   PIC X.
201005*<<<<<
100506        10  PI-DRAN0035-PZM-VFPHASEDAT     PIC X(10).

      *
       01  PO-DRAN0035.
           05 PO-DRAN0035-RC                   PIC  X(003).
             88 PO-DRAN0035-RC-OK                         VALUE 'OK '.
             88 PO-DRAN0035-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
