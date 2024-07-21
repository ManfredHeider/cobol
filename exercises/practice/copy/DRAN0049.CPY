      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0049.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAN0049.CPv  $
      *
      *   Rev 2.6   Oct 24 2013 10:59:58   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV + 
      *ADVC (Dialog)
      *
      *   Rev 2.5   Mar 24 2009 10:56:08   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.4   Mar 11 2009 16:12:00   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.3   23 Nov 2007 10:18:08   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 10 AM-Vers -
      *
      *   Rev 2.2   01 Sep 2006 09:43:44   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.1   Apr 23 2002 16:59:54   t13ae34
      *AAN00357-POA-AMV
      *
      *   Rev 2.0   Feb 27 1999 11:05:40   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:38:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0049  FUNKTION LIST-OZP FÜR TBAN0003 POAA*
      *                           GES, VORSCHLAGSNUMMER, LFD-NR      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00357!18.03.02! 2.1! NEUERSTELLUNG             ! HEIDER   *
      * AAN00730!30.11.07!2.3 ! ALLFINANZ                 ! RAHNER   *
      * AAN00843!11.03.09!2.5 ! VERGRÖßERN XGDV-FELD POA  !RAHNER    *
      * AAN01073!24.10.13!2.6 ! SEPA                      !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0049                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0049.
           05 PI-DRAN0049-PROGRAMM             PIC  X(008).
           05 PI-DRAN0049-UFKT-CODE            PIC  9(002).
           05 PI-DRAN0049-AUSWAHL-KRITERIEN.
               10 PI-DRAN0049-PGEB-GES         PIC S9(4) COMP.
               10 PI-DRAN0049-POA-HVANTNUM     PIC X(9).
      *
        01 PO-DRAN0049.
           05 PO-DRAN0049-RC                   PIC  X(003).
             88 PO-DRAN0049-RC-OK                        VALUE 'OK '.
             88 PO-DRAN0049-RC-LEER                      VALUE 'LER'.
             88 PO-DRAN0049-RC-EOF                       VALUE 'EOF'.
      *    POA-DATEN AUS PC-DATEN
           COPY DRAN0056.
      *
      ***** ENDE-COPY-MEMBER *****************************************
