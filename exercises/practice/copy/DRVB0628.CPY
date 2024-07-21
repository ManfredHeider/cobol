      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0628.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0628.CPv  $
      *
      *   Rev 2.1   Jan 14 2003 09:27:14   t13ae13
      *AVB00649: Neues Aufstiegssystem für RGS - D
      *
      *   Rev 2.0   Dec 04 2002 12:21:18   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0628                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0628                   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: WALB                                            *
      *    DATUM:    22.11.2002                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00649!12.12.02!2.1 ! NEU                       ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0628
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0628.
           05  PI-DRVB0628-MA-ST-NR           PIC  S9(009)    COMP.
           05  PI-DRVB0628-STUFE              PIC  S9(004)    COMP.
      *
       01  PO-DRVB0628.
           05  PO-DRVB0628-RC                 PIC  X(003).
               88 PO-DRVB0628-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0628-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0628-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0628-RC-ERR                       VALUE 'ERR'.

           05  PO-DRVB0628-ANZ-UNTST          PIC  9(004).
      *
      ******* ENDE COPY-MEMBER **************************************
