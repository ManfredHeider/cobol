      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.03.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0896.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0896.CPv  $
      *
      *   Rev 2.1   May 20 2008 12:12:54   t13ae48
      *AVB00875: Verlängerung der Adressfelder und Kontaktdaten - P
      *rogramme (JMS) - 3
      *
      *   Rev 2.0   Mar 19 2008 15:12:44   t13ae48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0896                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.MAESS                                          *
      *    DATUM:    19.03.2008                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00875!19.03.08! 01 ! Neuerstellung             !J.MAESS   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0896                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0896.
          05 PI-DRVB0896-MAAN-BEZUGS-NR     PIC S9(009) COMP.
      *
       01 PO-DRVB0896.

          05 PO-DRVB0896-RC                 PIC  X(003).
             88 PO-DRVB0896-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0896-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0896-DATEN.
             10 PO-DRVB0896-KNR-MAAN-TYP     PIC  S9(04) COMP .
             10 PO-DRVB0896-MAAN-STRASSE     PIC  X(040)      .
             10 PO-DRVB0896-MAAN-PLZ-AKTUELL PIC  X(009)      .
             10 PO-DRVB0896-MAAN-ORT         PIC  X(040)      .
             10 PO-DRVB0896-AUSL             PIC  S9(04) COMP .
             10 PO-DRVB0896-KNR-BEZ-KURZ     PIC  X(04)       .
             10 PO-DRVB0896-MAAN-TELEFON     PIC  X(026)      .
             10 PO-DRVB0896-MAAN-TELEFAX     PIC  X(026)      .
             10 PO-DRVB0896-KNR-GRND         PIC  S9(04) COMP .


      *
      ***** ENDE COPY-MEMBER DRVB0896 *****************************
