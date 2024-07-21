      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1134.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1134.CPv  $
      *
      *   Rev 2.0   May 26 1999 13:52:10   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 13:59:30   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************
      *    COPY-MEMBER: DRPA1134 FUNKTIONEN INIT VORVERTRAGSDATEN -  *
      *                                          MODUL               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                       *
      *    DATUM:    24.11.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!24.11.98! 01  !NEUERSTELLUNG             ! BOHLINGER*
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1134                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1134.
          05  PI-DRPA1134-UFKT                 PIC  X(002).
              88 PI-DRPA1134-INIT-TS-QUEUE              VALUE '01'.

      *
       01 PO-DRPA1134.
          05 PO-DRPA1134-RC                    PIC  X(003).
             88 PO-DRPA1134-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1134-RC-NOK                      VALUE 'NOK'.

      *
      ******** Ende Copy-Member **************************************
