      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0023.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/WUKI0023.CPv  $
      *
      *   Rev 2.6   Jun 11 2003 13:06:12   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:32:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-HAUSWOHNEIGENTUM.
           10 HWEPERSONENID               PIC X(13).
           10 HWEPERSONENROLLE            PIC X(1).
           10 HWELAUFENDENUMMER           PIC S9(9) COMP.
           10 HWEEIGTUMARTID              PIC X(3).
           10 HWEEIGTUMNARTID             PIC X(3).
           10 HWEABSCHRFUERANL            PIC X(30).
           10 NIL-HWEABSCHRFUERANL        PIC X(01).
           10 HWEBAUKINDERGELD            PIC S9(9)V9(2) COMP-3.
           10 NIL-HWEBAUKINDERGELD        PIC X(01).
           10 HWEBAUKINDGWKZ              PIC X(1).
           10 HWEBAUKINDGELDSEIT          PIC X(010).
           10 NIL-HWEBAUKINDGELDSEIT      PIC X(01).
           10 HWEEIGENTUMSEIT             PIC X(010).
           10 NIL-HWEEIGENTUMSEIT         PIC X(01).
           10 HWEMONNEBENKOSTEN           PIC S9(9)V9(2) COMP-3.
           10 NIL-HWEMONNEBENKOSTEN       PIC X(01).
           10 HWEMONNEBKOSTWKZ            PIC X(1).
           10 HWEMONERTRAG                PIC S9(9)V9(2) COMP-3.
           10 NIL-HWEMONERTRAG            PIC X(01).
           10 HWEMONERTRAGWKZ             PIC X(1).
           10 HWEOBJEKTWERT               PIC S9(9)V9(2) COMP-3.
           10 NIL-HWEOBJEKTWERT           PIC X(01).
           10 HWEOBJKETWERTWKZ            PIC X(1).
           10 HWESELBSTGENFLAE            PIC S9(4)V9(2) COMP-3.
           10 NIL-HWESELBSTGENFLAE        PIC X(01).
           10 HWESONDAUSGABEN             PIC S9(9)V9(2) COMP-3.
           10 NIL-HWESONDAUSGABEN         PIC X(01).
           10 HWESONDAUSGWKZ              PIC X(1).
           10 HWEWERBKOSTEN               PIC S9(9)V9(2) COMP-3.
           10 NIL-HWEWERBKOSTEN           PIC X(01).
           10 HWEWERBKOSTENWKZ            PIC X(1).
           10 HWEEIGENHEIMZULAGE          PIC S9(9)V9(2) COMP-3.
           10 NIL-HWEEIGENHEIMZULAGE      PIC X(01).
           10 HWEEIGHEIMZULWKZ            PIC X(1).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 HWEKAUFBAUKOSTEN            PIC S9(9)V9(2) COMP-3.
           10 NIL-HWEKAUFBAUKOSTEN        PIC X(01).
           10 HWEKAUFBAUKOSTWKZ           PIC X(1).
      *****************************************************************
