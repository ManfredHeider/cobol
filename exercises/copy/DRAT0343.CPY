      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0343.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0343.CPv  $
      *
      *   Rev 2.1   18 Jan 2001 14:15:14   T13EX43
      *APA00128 : Migration BfG-Invest und BfG-ImmoInvest
      *
      *   Rev 2.0   Mar 16 1999 09:57:30   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0343  FUNKTION PRUEFZIFFER BERECHNEN     *
      *                                    FUER BFG-IMMO             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    16.01.2001                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      *                                                              *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00128!16.01.01!2.1  ! NEU                       !KLEEMEIER*
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
          01 PI-DRAT0343.
           05 PI-DRAT0343-DATEN.
            10 PI-DRAT0343-PGEB-GES    PIC S9(004) COMP.
            10 PI-DRAT0343-ZAHL        PIC  9(015).
      *
          01 PO-DRAT0343.
           05 PO-DRAT0343-RC           PIC  X(003).
              88 PO-DRAT0343-RC-OK     VALUE 'OK '.
              88 PO-DRAT0343-RC-NOK    VALUE 'NOK'.
           05 PO-DRAT0343-DATEN.
            10 PO-DRAT0343-RC-MOD-11   PIC  X(003).
            10 PO-DRAT0343-PZ-MOD-11   PIC  9(001).

