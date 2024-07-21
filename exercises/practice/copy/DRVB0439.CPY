      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0439.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0439.cpv  $
      *
      *   Rev 2.1   Sep 02 1999 12:09:56   T13AE30
      * 
      *
      *   Rev 2.0   Aug 04 1999 10:30:10   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0439                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS ZWVB0045                             *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    23.08.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!23.08.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0439                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0439.
          05 PI-DRVB0439-ED-ST-NR              PIC S9(009) COMP.
      *
       01 PO-DRVB0439.

          05 PO-DRVB0439-RC                    PIC  X(003).
             88 PO-DRVB0439-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0439-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0439-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0439-DATEN.
             10 PO-DRVB0439-EDT-DAT-VON         PIC  X(010).
             10 PO-DRVB0439-EDT-DAT-BIS         PIC  X(010).

      *
      ***** ENDE COPY-MEMBER DRVB0392 *****************************


