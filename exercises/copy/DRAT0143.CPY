      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0143.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0143.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   10 Jul 1997 12:36:48   T13AE34
      *AFPA0021 - s. letzter put
      *
      *   Rev 1.1   08 Jul 1997 08:55:42   T13AE34
      *AFPA0021 - CICS-Name / Verzweigung Vorschalt-PG
      *
      *   Rev 1.0   Feb 19 1997 12:21:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *****************************************************************
      *    COPY-MEMBER: DRAT0143                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0143                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                           *
      *    DATUM:    04.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0021!04.07.97!    ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0143: LESEN TBAT0008  KNR
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRAT0143.
           05  PI-DRAT0143-DATEN.
               10 PI-DRAT0143-KTY-NR           PIC S9(004) COMP.
               10 PI-DRAT0143-KNR-BEZ10        PIC  X(010).
      *
       01  PO-DRAT0143.
           05  PO-DRAT0143-RC                  PIC  X(003).
               88 PO-DRAT0143-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0143-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRAT0143-DATEN.
               10 PO-DRAT0143-KNR-BEZ-KURZ     PIC  X(004).
               10 PO-DRAT0143-KNR-BEZ          PIC  X(080).
               10 PO-DRAT0143-KNR-NR           PIC S9(004) COMP.
      *
