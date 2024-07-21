      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1701.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1701.CPv  $
      *
      *   Rev 2.2   Apr 23 2012 12:45:12   t13ex48
      *AKI01117: DB-Änderungen R. 6.16: OZPs, COPYs TCKI0077
      *
      *   Rev 2.1   Jul 27 2004 13:10:26   t13ex27
      *AKI00427: Ladedaten für die Tabelle KUNDENUEBERTRAGUNG
      *
      *   Rev 2.0   Jul 07 2004 16:22:24   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DBO-9310!12.10.21!    ! KDUBEARBEITER von 8 auf 30! t13ex78  *
      *         !        !    ! Stellen erweitert         !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *      
       01 PI-DRKI1701.
           05  PI-DRKI1701-PROGRAMM             PIC  X(008).
           05  PI-DRKI1701-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05 PI-DRKI1701-KEY.
             10 PI-DRKI1701-VERTRAGSID       PIC  X(013).
      *
       01  PO-DRKI1701.
           05  PO-DRKI1701-RC                 PIC  X(003).
               88 PO-DRKI1701-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1701-RC-EOF                       VALUE 'EOF'.
               88 PO-DRKI1701-RC-LER                       VALUE 'LER'.
      *
           05  PO-DRKI1701-DATEN.


           10 PO-DRKI1701-KDULAUFENDENUMMER    PIC S9(4) COMP.
           10 PO-DRKI1701-LETZTEAENDERUNG      PIC X(26).
           10 PO-DRKI1701-VBALTID              PIC X(13).
           10 PO-DRKI1701-VBALTROLLENKZ        PIC X(1).
           10 PO-DRKI1701-VBNEUID              PIC X(13).
           10 PO-DRKI1701-VBNEUROLLENKZ        PIC X(1).
           10 PO-DRKI1701-DTAERLEDIGTKZ        PIC X(1).
           10 PO-DRKI1701-KDUEBERTRAGUNGSART   PIC X(1).
           10 PO-DRKI1701-KDUGUELTIGABDAT      PIC X(10).
           10 PO-DRKI1701-KDUBEARBEITER        PIC X(30).
           10 PO-DRKI1701-KDUEIGENVERMKZ       PIC X(1).
           10 PO-DRKI1701-KDUKKVLID            PIC X(13).
      ******************************************************************
