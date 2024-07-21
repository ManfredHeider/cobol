      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.05.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1803.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1803.CPv  $
      *
      *   Rev 2.2   04 Jun 2008 16:04:02   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben Datei-Copys
      *
      *   Rev 2.1   Oct 18 2005 11:40:36   t13ex49
      *AKI00483: Riester Förderung - Sourcen und OPC
      *
      *   Rev 2.0   May 17 2005 15:02:38   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      * AML - Riester - in der Ermittlung abgewiesene Sätze
      *
      * Logbuch
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00483!17.05.05!    ! NEU                       !Peter Pohl*
      * -------------------------------------------------------------*
      * AKI00711!09.04.08!2.2 ! neu Dauerzulagen-KZ       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  DRKI1803.
      *
              20 DRKI1803-SA                          PIC X(01).
              20 DRKI1803-VNR                         PIC 9(10).
              20 DRKI1803-Zul-Datum                   PIC 9(08).
              20 DRKI1803-Zul-Beitragsjahr            PIC 9(04).
              20 DRKI1803-Zul-Wert                    PIC S9(13)V99.
              20 DRKI1803-WKZ                         PIC X(03).
090408        20 DRKI1803-DAUERZUL-KZ                 PIC X(01).
      *
      *------------------------- Ende DRKI1803 ------------------
