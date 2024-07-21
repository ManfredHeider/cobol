      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1115.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1115.CPv  $
      *
      *   Rev 2.1   10 Aug 2000 16:05:26   T13EX39
      *AKI00136 BFG-KUNDENÜBERTRAGUNG
      *
      *   Rev 2.1   06 Jul 2000 13:45:16   T13EX39
      *AKI00100 BFG-INVEST
      *
      *   Rev 2.0   Feb 27 1999 12:53:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:52:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1115 FUNKTION:VERTRAGSDATEN ERMITTELN    *
      *                                   LIST                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO
      *    DATUM:     5.06.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00136!03.08.00!1.0 ! zus. DECLARE CURSOR       ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1115                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION:                                                 *
      *              - ALLE GUTHABENBUCHUNGEN ZU EINER GESELLSCHAFT  *
      *                ERMITTELN                                     *
      *                                                              *
      ****************************************************************
       01 PI-DRKI1115.
           05  PI-DRKI1115-PROGRAMM             PIC  X(008).
           05  PI-DRKI1115-UFKT                 PIC  X(002).
           05  PI-DRKI1115-VERVERTRAGSNUM-1  PIC  X(15).
           05  PI-DRKI1115-VERVERTRAGSNUM-2  PIC  X(15).
           05  PI-DRKI1115-VERPARTGESELLSCHID   PIC  S9(04) COMP.
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
      ***
       01 PO-DRKI1115.
          05 PO-DRKI1115-RC                     PIC  X(003).
             88 PO-DRKI1115-RC-OK                        VALUE 'OK '.
             88 PO-DRKI1115-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI1115-RC-EOF                       VALUE 'EOF'.
           05  PO-DRKI1115-DATEN.
             10 PO-DRKI1115-VERVERTRAGSID       PIC X(13).
             10 PO-DRKI1115-VERVERTRAGSNUMMER   PIC X(15).
      *
      ******** Ende Copy-Member **************************************
