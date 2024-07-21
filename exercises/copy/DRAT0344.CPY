      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0344.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0344.CPv  $
      *
      *   Rev 2.1   07 Feb 2001 13:11:22   T13EX21
      *APA00118 - Neuerstellung
      *
      *   Rev 2.0   Mar 16 1999 09:57:30   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0344                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0344                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    07.01.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!07.01001!    ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0344: LESEN TBAT0008  MAX (KNR)
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRAT0344.
           05  PI-DRAT0344-DATEN.
               10 PI-DRAT0344-KTY-NR           PIC S9(004) COMP.
               10 PI-DRAT0344-KNR-BEZ10        PIC  X(010).
      *
       01  PO-DRAT0344.
           05  PO-DRAT0344-RC                  PIC  X(003).
               88 PO-DRAT0344-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0344-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRAT0344-DATEN.
               10 PO-DRAT0344-KNR-BEZ-KURZ     PIC  X(004).
               10 PO-DRAT0344-KNR-BEZ          PIC  X(080).
               10 PO-DRAT0344-KNR-NR           PIC S9(004) COMP.
      *
