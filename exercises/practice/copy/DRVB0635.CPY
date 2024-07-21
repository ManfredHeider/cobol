      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0635.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0635.CPv  $
      *
      *   Rev 2.1   Feb 11 2003 11:55:14   t13ex57
      *AAS00387 - DIM 7 Zukunft Nadeln
      *
      *   Rev 2.0   Dec 04 2002 12:21:48   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0635                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0635                   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: WALB                                            *
      *    DATUM:    10.02.2003                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00387!10.02.03!2.1 ! NEU                       ! U. BOLL  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0635
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0635.
           05  PI-DRVB0635-MA-ST-NR           PIC  S9(009)    COMP.
           05  PI-DRVB0635-STUFE              PIC  S9(004)    COMP.
           05  PI-DRVB0635-STICHTAG           PIC  X(010).
      *
       01  PO-DRVB0635.
           05  PO-DRVB0635-RC                 PIC  X(003).
               88 PO-DRVB0635-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0635-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0635-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0635-RC-ERR                       VALUE 'ERR'.

           05  PO-DRVB0635-ANZ-UNTST          PIC  9(004).
      *
      ******* ENDE COPY-MEMBER **************************************
