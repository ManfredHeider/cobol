      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0139.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0139.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Jul 1997 08:54:12   T13AE34
      *AFPA0021 - CICS-Name / Verzweigung Vorschalt-PG
      *
      *   Rev 1.0   Feb 19 1997 12:21:12   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0139                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0139                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                        *
      *    DATUM:    30.05.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0013!30.05.97!    ! NEU                       ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0139: LESEN TBAT0008  KNR
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRAT0139.
           05  PI-DRAT0139-DATEN.
               10 PI-DRAT0139-KTY-NR           PIC S9(004) COMP.
               10 PI-DRAT0139-KNR-BEZ-KURZ     PIC  X(004).
      *
       01  PO-DRAT0139.
           05  PO-DRAT0139-RC                  PIC  X(003).
               88 PO-DRAT0139-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0139-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRAT0139-DATEN.
               10 PO-DRAT0139-KNR-NR           PIC S9(004) COMP.
               10 PO-DRAT0139-KNR-BEZ10        PIC  X(010).
      *
