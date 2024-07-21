      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0415.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0415.cpv  $
      *
      *   Rev 2.1   Jun 10 1999 11:17:38   T13AE34
      *APA00012-SST Daisy
      *
      *   Rev 2.0   May 27 1999 13:58:30   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0415                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO ZWVB0072                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                           *
      *    DATUM:    04.06.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!28.05.99!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FUER DIE FUNKTION DRVB0415                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0415.
          05 PI-DRVB0415-KNR-STEUER-INFO    PIC S9(004) COMP.

      *
       01 PO-DRVB0415.

          05 PO-DRVB0415-RC                 PIC  X(003).
             88 PO-DRVB0415-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0415-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0415-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0415-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0415-DATEN.
             10 PO-DRVB0415-ZEITPUNKT        PIC X(026).
             10 PO-DRVB0415-LAST-UPDATE      PIC X(026).
             10 PO-DRVB0415-KENNUNG          PIC X(008).

      *
      ***** ENDE COPY-MEMBER DRVB0415 *****************************
