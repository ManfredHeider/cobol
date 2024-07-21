      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1177.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1177.CPv  $
      *
      *   Rev 2.0   May 26 1999 16:03:46   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 14:03:38   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1177  Funktion Select Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: LÜTTGES                                         *
      *    DATUM:    24.02.1999                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!24.02.99! 01 ! Neuerstellung             ! LÜTTGES  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1177                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  PRÜFEN VSTATUS IN VGRUPPE <<<    PVSG TBPA0016       *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1177.
          05 PI-DRPA1177-PVSG-KEY.
             10 PI-DRPA1177-PVSG-VSTAT-GRP    PIC S9(004) BINARY.
             10 PI-DRPA1177-PVST-VSTAT        PIC  X(004).
      *
       01 PO-DRPA1177.
          05 PO-DRPA1177-RC                   PIC  X(003).
             88 PO-DRPA1177-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1177-RC-VST-IN-VGR-NFD            VALUE 'VNF'.
      *
      ****** Ende Copy-Member *****************************************
