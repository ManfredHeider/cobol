      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2018.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2018.CPv  $
      *
      *   Rev 2.2   Dec 31 2007 17:58:02   t13ex48
      *AKL00666: Anpassung RUECK-DTA für Allfinanz PGM ADA
      *
      *   Rev 2.1   Apr 04 2007 15:55:28   t13ex48
      *AKI00557: Ticketverfahren OKI: RUECK-DTA Adressaenderungen
      *
      *   Rev 2.0   Feb 07 2007 15:29:12   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI2018  PI/PO RDTA Adressaenderung         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Sebbach                                         *
      *    DATUM:    07.02.2007                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!07.02.07! 01 ! NEUERSTELLUNG             ! Sebbach  *
      *---------!--------!----!---------------------------!----------*
      * AKL00666!19.12.07! 02 ! Neues Feld Mandanten-Nr   ! Sebbach  *
      *---------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *---------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI2018                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  ADRESSAENDERUNGSDATEN ERMITTELN    <<<               *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI2018.
          05 PI-DRKI2018-KEY.
             10 PI-DRKI2018-HADPERSONENID           PIC X(13).
             10 PI-DRKI2018-HADVERTRAGSID           PIC X(13).
             10 PI-DRKI2018-HADADRESSEALTID         PIC X(13).
      *
       01 PO-DRKI2018.
          05 PO-DRKI2018-RC                 PIC  X(003).
             88 PO-DRKI2018-RC-OK                VALUE 'OK '.
             88 PO-DRKI2018-RC-NFD               VALUE 'NFD'.
             88 PO-DRKI2018-RC-EOF               VALUE 'EOF'.
             88 PO-DRKI2018-RC-FEHLPROTOKOLL     VALUE 'FP '.
          05 PO-DRKI2018-DATEN.
             10 PO-DRKI2018-PARTGESELLSCHAFTID      PIC 9(003).
             10 PO-DRKI2018-VERTRAGSNR              PIC 9(015).
             10 PO-DRKI2018-DIREKTIONSNR            PIC 9(003).
             10 PO-DRKI2018-GESCHAEFTSSTELLE        PIC X(010).
             10 PO-DRKI2018-KUNDENBETREUERNR        PIC 9(007).
             10 PO-DRKI2018-KUNDENNAMEALT           PIC X(030).
             10 PO-DRKI2018-KUNDENVNAMEALT          PIC X(015).
             10 PO-DRKI2018-STRASSEALT              PIC X(045).
             10 PO-DRKI2018-PLZALT                  PIC X(015).
             10 PO-DRKI2018-ORTALT                  PIC X(030).
             10 PO-DRKI2018-LANDKZALT               PIC X(003).
             10 PO-DRKI2018-KUNDENNAMENEU           PIC X(030).
             10 PO-DRKI2018-KUNDENVNAMENEU          PIC X(015).
             10 PO-DRKI2018-ANREDEKZNEU             PIC 9(002).
             10 PO-DRKI2018-PERSONARTKZNEU          PIC X(001).
             10 PO-DRKI2018-TELEFONNRNEU            PIC X(128).
             10 PO-DRKI2018-GEBGRUENDDAT            PIC X(010).
191207       10 PO-DRKI2018-MANDANT                 PIC 9(009).
      *
      ****** Ende Copy-Member ****************************************
