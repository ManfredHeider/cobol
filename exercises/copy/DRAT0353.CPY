      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0353.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0353.CPv  $
      *
      *   Rev 2.1   10 May 2001 13:11:30   T13AE34
      *ACV00015-CTV-Briefe aus Neudialogen
      *
      *   Rev 2.0   01 Mar 2001 13:03:12   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0353                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0353                    *
      *    BEANTWORTET DIE FRAGE, OB ZU KTY UND BEZ-KURZ              *
      *    EIN (RC-OK) ODER MEHRERE EINTRÄGE (RC-MULT-ROWS) EXISTIEREN*
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                           *
      *    DATUM:    09.03.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ACV00015!08.03.01!2.1 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0353: LESEN TBAT0008  KNR
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRAT0353.
           05  PI-DRAT0353-DATEN.
               10 PI-DRAT0353-KTY-NR           PIC S9(004) COMP.
               10 PI-DRAT0353-KNR-BEZ-KURZ     PIC  X(004).
      *
       01  PO-DRAT0353.
           05  PO-DRAT0353-RC                  PIC  X(003).
               88 PO-DRAT0353-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0353-RC-MULT-ROWS                 VALUE 'MUR'.
               88 PO-DRAT0353-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRAT0353-DATEN.
               10 FILLER                       PIC  X(001).
      *
