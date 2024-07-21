      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0013.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRSL0013.CPv  $
      *
      *   Rev 2.1   Jul 28 2005 14:17:42   t13ex13
      *ACV00070: CTV-Brief Anwärter Karriere Seminar
      *
      *   Rev 2.0   Jun 07 2005 17:57:52   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME     *
      * --------------------------------------------------------------*
      * ACV00070!07.06.05!2.0 ! NEU                        !Gutfleisch*
      *         !        !    !                            !          *
      *         !        !    !                            !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *            INHALT    :  CTV-DATEI Anwärter Karriere Seminar   *
      *            DATEI     :  CV002200.VBNR                         *
      *            MEMBER    :  DRSL0013                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  33 BYTE                                *
      *            KEY       :  VBNR (1-7)                            *
      *                                                               *
      *---------------------------------------------------------------*

       01  DRSL0013-SATZ.
           05 DRSL0013-VBNR                     PIC 9(007).
           05 DRSL0013-IST-EINH                 PIC X(009).
           05 DRSL0013-FEHL-EINH                PIC X(009).
           05 DRSL0013-STICHTAG                 PIC X(7).
           05 DRSL0013-KATEGORIE                PIC X(1).
      *****
      *****   ENDE DRSL0013
      *****
