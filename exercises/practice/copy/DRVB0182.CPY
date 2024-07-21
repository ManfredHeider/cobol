      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0182.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0182.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 11:44:06   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   23 Jul 1997 12:11:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0182                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION drvb0182                    *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00167!17.10.97!    ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION drvb0182
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0182.
           05  PI-DRVB0182-KEY.
               10 PI-DRVB0182-MA-ST-NR        PIC S9(009) COMP.
      *
       01  PO-DRVB0182.
           05  PO-DRVB0182-RC                 PIC  X(003).
               88 PO-DRVB0182-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0182-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0182-KNR-AV-TYP         PIC S9(004) BINARY.
           05  PO-DRVB0182-AV-DAT-BIS         PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
