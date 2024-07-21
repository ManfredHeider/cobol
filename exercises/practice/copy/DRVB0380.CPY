      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0380.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRVB0380.CPv  $
      *
      *   Rev 2.1   Jan 14 2003 09:26:36   t13ae13
      *AVB00649: Neues Aufstiegssystem für RGS - D
      *
      *   Rev 2.0   Mar 17 1999 10:59:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0380                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0380                   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: WALB                                            *
      *    DATUM:    22.11.2002                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00649!22.11.02!2.1 ! NEU                       ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0380
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0380.
           05  PI-DRVB0380-MA-ST-NR           PIC  S9(009)    COMP.
           05  PI-DRVB0380-ANZ-MONATE         PIC  S9(004)    COMP.
           05  PI-DRVB0380-ANZ-EINHEITEN      PIC  S9(009)V9  COMP-3.
           05  PI-DRVB0380-STUFE              PIC  S9(004)    COMP.
      *
       01  PO-DRVB0380.
           05  PO-DRVB0380-RC                 PIC  X(003).
               88 PO-DRVB0380-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0380-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0380-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0380-RC-ERR                       VALUE 'ERR'.

           05  PO-DRVB0380-ANZ-UNTST-HPTB     PIC  9(004).
      *
      ******* ENDE COPY-MEMBER **************************************
