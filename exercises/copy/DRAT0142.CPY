      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0142.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0142.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   10 Jul 1997 11:28:40   T13EX11
      *AFPA0019 - Neuerstellung / Funktion fuer UPAT0017
      *
      *   Rev 1.0   Feb 19 1997 12:21:38   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0142                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0142                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHÖLLHAMMER                                  *
      *    DATUM:    10.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0019!10.06.97! 01 ! NEU                       ! SCHÖLLH. *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0142:
      *            'ÜBERPRÜFUNG KOMPETENZ BEI TRANSAKTIONSWECHSEL'
      *            (DIESE FUNKTION LÖST DAS PGAT0012 AB)
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRAT0142.
           05  PI-DRAT0142-DATEN.
               10 PI-DRAT0142-USERID             PIC  X(007).
               10 PI-DRAT0142-TRANSID            PIC  X(004).
               10 PI-DRAT0142-USITZG-ANZ         PIC  9(001).
      *
       01  PO-DRAT0142.
           05  PO-DRAT0142-RC                    PIC  X(003).
               88 PO-DRAT0142-RC-TR-UNGUELT                VALUE 'UG '.
               88 PO-DRAT0142-RC-TR-NICHT-VORH             VALUE 'NV '.
               88 PO-DRAT0142-RC-NOK-BILDSCHIRM            VALUE 'BL '.
               88 PO-DRAT0142-RC-KEINE-KOMPETENZ           VALUE 'KK '.
               88 PO-DRAT0142-RC-TR-GESPERRT               VALUE 'SP '.
               88 PO-DRAT0142-RC-NOK-IM-UMODUS-V           VALUE 'NUV'.
               88 PO-DRAT0142-RC-NOK-IM-UMODUS-A           VALUE 'NUA'.
               88 PO-DRAT0142-RC-OK-ALT-ANW                VALUE 'AL '.
               88 PO-DRAT0142-RC-OK-ALT-ANWVERZ            VALUE 'ALV'.
               88 PO-DRAT0142-RC-OK-ANW                    VALUE 'NE '.
               88 PO-DRAT0142-RC-OK-UANW                   VALUE 'NEU'.
      *
      ******* ENDE COPY-MEMBER **************************************

