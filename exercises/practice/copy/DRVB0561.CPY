      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0561.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRVB0561.CPv  $
      *
      *   Rev 2.1   Apr 30 2002 09:13:44   t13ae27
      *zusätzliche Parameter in CTV-Briefen
      *
      *   Rev 2.0   15 May 2001 13:33:40   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0561                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0561                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. DUERR                                         *
      *    DATUM:    02.11.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ACV00007!02.11.01!    ! NEU                       !P.DUERR   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0561
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0561.
           05  PI-DRVB0561-MA-ST-NR           PIC S9(009) COMP.
           05  PI-DRVB0561-KTY-MAST-TYP       PIC S9(004) COMP.
           05  PI-DRVB0561-KNR-MAST-TYP       PIC S9(004) COMP.
      *
       01  PO-DRVB0561.
           05  PO-DRVB0561-RC                 PIC  X(003).
               88 PO-DRVB0561-RC-OK                  VALUE 'OK '.
               88 PO-DRVB0561-RC-NOK                 VALUE 'NOK'.
               88 PO-DRVB0561-RC-NOT-FOUND           VALUE 'NFD'.
               88 PO-DRVB0561-RC-ERR                 VALUE 'ERR'.
      *
           05  PO-DRVB0561-MAST-DAT-VON       PIC X(026).
           05  PO-DRVB0561-MAST-DAT-BIS       PIC X(010).  
      *********************** ENDE COPY-MEMBER *************************
