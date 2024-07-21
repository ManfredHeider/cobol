      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0333.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0333.CPv  $
      *
      *   Rev 2.1   Mar 01 2017 15:26:46   t13ex27
      *AKI01490: Tabelle Produkt aufräumen
      *
      *   Rev 1.1   09 May 1998 13:29:16   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:58:22   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE PRODUKT TABLE
CCC004*    ( PRODUKTID                      SMALLINT NOT NULL,
CCC004*      PRODUKTTEXT                    CHAR(72) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE PRODUKT                     *
      ******************************************************************
       01  PRODUKT-GESAMT.
        05 NI-PRODUKT-TAB.
           10 NI-PRODUKT-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05 NI-PRODUKT REDEFINES NI-PRODUKT-TAB.
           10 NI-PRODUKTID          PIC S9(4) COMP.
           10 NI-PRODUKTTEXT        PIC S9(4) COMP.
        05 DCL-PRODUKT.
           10 PRODUKTID            PIC S9(4) USAGE COMP.
           10 PRODUKTTEXT          PIC X(72).
      ************************** ENDE-DCLGEN *************************

