      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.03.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1071.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB1071.CPv  $
      *
      *   Rev 2.3   Apr 21 2015 14:02:28   t13ex27
      *AKI01352: VB-Nummer Prüfung 'neu' gestalten
      *
      *   Rev 2.2   Apr 16 2013 13:38:56   t13ex39
      *AKI01190: Bereinigung des Verwaltungsfonds
      *
      *   Rev 2.1   Apr 15 2013 12:54:36   t13ex39
      *AKI01190: Bereinigung des Verwaltungsfonds
      *
      *   Rev 2.0   Mar 26 2013 12:25:12   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AKI01190!02.04.13!1.0 ! NEU                       ! WHO      *
      * --------+--------+----+---------------------------+----------*
      * AKI01352!03.03.15!2.3 ! Neue PPI-Attribute        ! IFR      *
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
       01  PI-DRVB1071.
           05  PI-DRVB1071-KEY.
             10 PI-DRVB1071-VBNR            PIC S9(9) COMP.
030315       10 PI-DRVB1071-STRUKTURSTUFE   PIC S9(9) COMP.
030315       10 PI-DRVB1071-MSBZ-DAT-BIS    PIC  X(10).
      *
       01  PO-DRVB1071.
           05  PO-DRVB1071-KZ                 PIC  X(001).
           05  PO-DRVB1071-RC                 PIC  X(003).
               88 PO-DRVB1071-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1071-RC-NOT-FOUND                 VALUE 'NFD'.

