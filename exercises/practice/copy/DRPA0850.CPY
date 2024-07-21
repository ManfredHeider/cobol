      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0850.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0850.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 11:11:22   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:46:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0850                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0850                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Parent                                           *
      *    DATUM:    10.02.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * Daisy   !10.02.98!01.00! NEU                      !  Parent  *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    dieses copy enthÄlt die ein- und ausgabeparameter
      *    fÜr die funktion drpa0850: lesen maximale version einer printe
      *    ausser: dem funktionsnamen,
      *            dem err-error-bereich und
      *            dem standard-status-bereich
      *            (jeweils separate copys)
      *
      ****************************************************************
      *
       01  PI-DRPA0850.
           05  PI-DRPA0850-DATEN.
               10 PI-DRPA0850-PRINTE        PIC S9(015) PACKED-DECIMAL.
      *
       01  PO-DRPA0850.
           05  PO-DRPA0850-RC               PIC  X(003).
               88 PO-DRPA0850-RC-OK              VALUE 'OK '.
               88 PO-DRPA0850-RC-NOT-FOUND       VALUE 'NFD'.
           05  PO-DRPA0850-VERSION          PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
