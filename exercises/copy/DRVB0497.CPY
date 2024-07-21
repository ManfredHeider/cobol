      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0497.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0497.CPv  $
      *
      *   Rev 2.1   13 Oct 2000 09:36:08   T13AE28
      * 
      *
      *   Rev 2.0   19 May 2000 09:50:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0497                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0497                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    08.08.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00490!08.08.00! 01 ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0497
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0497.
           05  PI-DRVB0497-PROV-STF           PIC  S9(003)V99.
      *
       01  PO-DRVB0497.
           05  PO-DRVB0497-RC                 PIC  X(003).
               88 PO-DRVB0497-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0497-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0497-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0497-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0497-VBP-NR             PIC  S9(004) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************


