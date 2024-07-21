      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0362.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0362.CPv  $
      *
      *   Rev 2.1   May 10 1999 12:06:38   T13AE42
      *AAS00127 - PI/PO fuer UPVB0107
      *
      *   Rev 2.0   May 10 1999 09:51:18   T13AE30
      * 
      *
      *   Rev 1.0   22 Jan 1999 13:40:52   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0362                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPVB0107                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    22.02.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00117!22.02.99! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0362                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0362.
          05 PI-DRVB0362-VBNR               PIC S9(009) COMP.
      *
       01 PO-DRVB0362.

          05 PO-DRVB0362-RC                 PIC  X(003).
             88 PO-DRVB0362-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0362-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0362-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0362-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0362-DATEN.
             10 PO-DRVB0362-BETR-NR         PIC S9(009)  COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0362 *****************************
