      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0606.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0606.CPv  $
      *
      *   Rev 2.1   16 May 2007 06:44:10   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.0   Mar 10 1999 12:11:14   T13EX22
      *MFE-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:54:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0606                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PEW ERMITTELN>>                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    12.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA00??!12.11.97! 01 ! NEU                       ! MUND     *
      * APA00604!12.04.07!2.1 ! ABSCHALTUNG PEW           ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0606                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0606.
          05 PI-DRPA0606-DATEN.
             07 PI-DRPA0606-PMPE.
                COPY DRPA0289.
             07 PI-DRPA0606-PMPP.
                COPY DRPA0290.
      *
       01 PO-DRPA0606.
          05 PO-DRPA0606-RC                  PIC  X(003).
120407*      88 PO-DRPA0606-RC-PEW-NFD-OK    VALUE 'PNO'.
120407*      88 PO-DRPA0606-RC-PEW-NFD-NOK   VALUE 'PNN'.
120407*      88 PO-DRPA0606-RC-PEW-FD-OK     VALUE 'PFO'.
120407*      88 PO-DRPA0606-RC-PEW-FD-NOK    VALUE 'PFN'.
120407       88 PO-DRPA0606-RC-ANTRAG-NFD-OK    VALUE 'ANO'.
120407       88 PO-DRPA0606-RC-ANTRAG-NFD-NOK   VALUE 'ANN'.
120407       88 PO-DRPA0606-RC-ANTRAG-FD-OK     VALUE 'AFO'.
120407       88 PO-DRPA0606-RC-ANTRAG-FD-NOK    VALUE 'AFN'.
120407*   05 PO-DRPA0606-DATEN.
120407*      10 PO-DRPA0606-PEW-ANTR.
120407*         COPY DRPA0434.
120407*      10 PO-DRPA0606-PEW-STRUK.
120407*         COPY DRPA0435.
      ******* ENDE COPY-MEMBER **************************************
