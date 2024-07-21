      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0673.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0673.CPv  $
      *
      *   Rev 2.1   Sep 04 2003 13:06:04   t13ex22
      *AVB00701:Dialog VB44 - Pflege Insolvenzkonten; 
      *Anzeige BV-Schutz in UU11 und VB21
      *
      *   Rev 2.0   Feb 17 2003 10:59:22   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0673                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0673                   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    02.09.2003                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00701!02.09.03!2.0 ! NEU                       !MUND      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0673
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0673.
           05  PI-DRVB0673-MA-ST-NR               PIC  9(009) COMP.
           05  PI-DRVB0673-KTY-MAST-TYP           PIC S9(004) COMP.
           05  PI-DRVB0673-KNR-MAST-TYP           PIC S9(004) COMP.
      *
       01  PO-DRVB0673.
           05  PO-DRVB0673-RC                     PIC  X(003).
               88 PO-DRVB0673-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0673-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0673-DATEN.
               10 PO-DRVB0673-MA-ST-NR            PIC S9(9) USAGE COMP.
               10 PO-DRVB0673-MAST-DAT-VON        PIC X(26).
               10 PO-DRVB0673-KTY-MAST-TYP        PIC S9(4) USAGE COMP.
               10 PO-DRVB0673-KNR-MAST-TYP        PIC S9(4) USAGE COMP.
               10 PO-DRVB0673-MAST-LAST-UPDATE    PIC X(26).
               10 PO-DRVB0673-MAST-DAT-BIS        PIC X(10).
      *
      ******* ENDE COPY-MEMBER **************************************
