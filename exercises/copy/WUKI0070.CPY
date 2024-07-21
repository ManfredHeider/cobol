      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0070.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUKI0070.CPv  $
      *
      *   Rev 2.3   Dec 06 2016 15:13:26   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.2   Nov 28 2016 12:16:14   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.1   Nov 28 2016 11:34:50   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 1.2   09 May 1998 14:12:32   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:06:06   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
        05  WU-VBSTAMMDATEN.
           10 VBSPERSONENID                 PIC  X(0013).
           10 VBSVORGESPERSID               PIC  X(0013).
           10 NIL-VBSVORGESPERSID           PIC  X(01).
           10 VBSMITARBEITERNR              PIC S9(9) COMP.
           10 VBSMANDANTNR                  PIC S9(9) COMP.
           10 VBSANREDE                     PIC S9(4) COMP.
           10 VBSNAME                       PIC  X(0040).
           10 VBSVORNAME                    PIC  X(0020).
           10 NIL-VBSVORNAME                PIC  X(01).
           10 VBSGEBGRUENDDAT               PIC  X(010).
           10 NIL-VBSGEBGRUENDDAT           PIC  X(01).
           10 VBSSTRUKTURSTUFE              PIC S9(9) COMP.
           10 NIL-VBSSTRUKTURSTUFE          PIC  X(01).
           10 VBSDIREKTION                  PIC S9(4) COMP.
           10 VBSPROVSATZ                   PIC S9(3)V9(2) COMP-3.
           10 NIL-VBSPROVSATZ               PIC  X(01).
           10 VBSAUSTRITTSDATUM             PIC  X(010).
           10 NIL-VBSAUSTRITTSDATUM         PIC  X(01).
           10 VBSRUECKSTUFDATUM             PIC  X(010).
           10 NIL-VBSRUECKSTUFDATUM         PIC  X(01).
           10 VBSUMSTUFDATUM                PIC  X(010).
           10 NIL-VBSUMSTUFDATUM            PIC  X(01).
           10 VBS34FDAT                     PIC  X(010).
           10 NIL-VBS34FDAT                 PIC  X(01).
           10 VBS34FREGISTNR                PIC  X(0015).
           10 NIL-VBS34FREGISTNR            PIC  X(01).
           10 VBS34IDAT                     PIC  X(010).
           10 NIL-VBS34IDAT                 PIC  X(01).
           10 VBS34CDAT                     PIC  X(010).
           10 NIL-VBS34CDAT                 PIC  X(01).
           10 VBSEDISTATUS                  PIC  X(0001).
           10 NIL-VBSEDISTATUS              PIC  X(01).
           10 VBSEDIDATUM                   PIC  X(010).
           10 NIL-VBSEDIDATUM               PIC  X(01).
           10 VBSZUSTELLPRIV                PIC S9(4) COMP.
           10 NIL-VBSZUSTELLPRIV            PIC  X(01).
           10 VBSZUSTELLGESCH               PIC S9(4) COMP.
           10 NIL-VBSZUSTELLGESCH           PIC  X(01).
           10 VBSFONPRIVAT                  PIC  X(0026).
           10 NIL-VBSFONPRIVAT              PIC  X(01).
           10 VBSFAXPRIVAT                  PIC  X(0026).
           10 NIL-VBSFAXPRIVAT              PIC  X(01).
           10 VBSMOBILPRIVAT                PIC  X(0026).
           10 NIL-VBSMOBILPRIVAT            PIC  X(01).
           10 VBSSTRASSENRPRIV              PIC  X(0040).
           10 NIL-VBSSTRASSENRPRIV          PIC  X(01).
           10 VBSPLZPRIV                    PIC  X(0009).
           10 NIL-VBSPLZPRIV                PIC  X(01).
           10 VBSORTPRIV                    PIC  X(0040).
           10 NIL-VBSORTPRIV                PIC  X(01).
           10 VBSAUSLPRIV                   PIC S9(4) COMP.
           10 NIL-VBSAUSLPRIV               PIC  X(01).
           10 VBSFONGESCH                   PIC  X(0026).
           10 NIL-VBSFONGESCH               PIC  X(01).
           10 VBSFAXGESCH                   PIC  X(0026).
           10 NIL-VBSFAXGESCH               PIC  X(01).
           10 VBSMOBILGESCH                 PIC  X(0026).
           10 NIL-VBSMOBILGESCH             PIC  X(01).
           10 VBSSTRASSENRGESCH             PIC  X(0040).
           10 NIL-VBSSTRASSENRGESCH         PIC  X(01).
           10 VBSPLZGESCH                   PIC  X(0009).
           10 NIL-VBSPLZGESCH               PIC  X(01).
           10 VBSORTGESCH                   PIC  X(0040).
           10 NIL-VBSORTGESCH               PIC  X(01).
           10 VBSAUSLGESCH                  PIC S9(4) COMP.
           10 NIL-VBSAUSLGESCH              PIC  X(01).
           10 VBSMANDANTNRALT               PIC S9(9) COMP.
           10 NIL-VBSMANDANTNRALT           PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).



