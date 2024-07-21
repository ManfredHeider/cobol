      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0297.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drat0297.cpv  $
      *
      *   Rev 2.1   Aug 02 1999 10:43:54   T13AE34
      *AAT00010-Neukonzeption KS10
      *
      *   Rev 2.0   Mar 16 1999 09:57:18   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0297                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0297                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    30.05.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00010!16.07.99!2.1 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0297: LESEN TBAT0008  KNR
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRAT0297.
           05  PI-DRAT0297-DATEN.
               10 PI-DRAT0297-KTY-NR           PIC S9(004) COMP.
               10 PI-DRAT0297-KNR-BEZ-KURZ     PIC  X(004).
               10 PI-DRAT0297-KNR-BEZ10        PIC  X(010).
      *
       01  PO-DRAT0297.
           05  PO-DRAT0297-RC                  PIC  X(003).
               88 PO-DRAT0297-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0297-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRAT0297-DATEN.
               10 PO-DRAT0297-KNR-NR           PIC S9(004) COMP.
               10 PO-DRAT0297-KNR-BEZ          PIC  X(080).
      *
