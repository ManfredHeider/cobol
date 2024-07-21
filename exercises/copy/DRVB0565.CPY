      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0565.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRVB0565.CPv  $
      *
      *   Rev 2.2   Jan 14 2003 09:24:42   t13ae13
      *AVB00649: Neues Aufstiegssystem für RGS - D
      *
      *   Rev 2.1   05 Dec 2001 09:08:44   t13ae13
      *AVB00544: Umstellung 40%-Klausel auf variabel
      *
      *   Rev 2.0   15 May 2001 13:33:44   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0565                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0565                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    12.11.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00544!12.11.01! 2.1! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0565
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0565.
           10 PI-DRVB0565-MS-NR               PIC  S9(004) COMP.
           10 PI-DRVB0565-KNR-GESELL-TYP      PIC  S9(004) COMP.
           10 PI-DRVB0565-KNR-BEFBED-TYP      PIC  S9(004) COMP.
      *
       01  PO-DRVB0565.
           05  PO-DRVB0565-RC                 PIC  X(003).
               88 PO-DRVB0565-RC-OK                VALUE 'OK '.
               88 PO-DRVB0565-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB0565-RC-NOT-FOUND         VALUE 'NFD'.
      *
           05  PO-DRVB0565-MS-ABZPROZ-REGEL   PIC S9(003)V99 COMP-3.
      ******* ENDE COPY-MEMBER **************************************

