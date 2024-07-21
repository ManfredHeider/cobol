      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0557.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRVB0557.CPv  $
      *
      *   Rev 2.1   Apr 30 2002 09:13:06   t13ae27
      *zusätzliche Parameter in CTV-Briefen
      *
      *   Rev 2.0   15 May 2001 13:33:36   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0557, ALLGEMEINE SCHNITTSTELLE           *
      *                 FÜR CTV-BATCH UPRO-CALL                      *
      *    --------------------------------------------------------  *
      *    ERSTELLT: P. Dürr                                         *
      *    DATUM:    06.08.2001                                      *
      ****************************************************************
      *    VERSION   1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ACV00007!06.08.01!    ! NEU                       ! P.DÜRR   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   BESCHREIBUNG DER SCHNITTSTELLE:
      *
      *   EINGABE:
      *
      *     PI-DRVB0557-ST-NR
      *
      *   AUSGABE:
      *
      *     PO-DRVB0557-RC
      *     PO-DRVB0557-DATUM           VM-VEREINBARUNG/VB-VERTRAG
      *
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRVB0557.
           05  PI-DRVB0557-ST-NR              PIC S9(009) COMP.

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRVB0557.
           05  PO-DRVB0557-RC                 PIC  X(003).
               88  PO-DRVB0557-RC-OK                      VALUE 'OK '.
               88  PO-DRVB0557-RC-NOK                     VALUE 'NOK'.
               88  PO-DRVB0557-RC-NOT-FOUND               VALUE 'NFD'.

           05  PO-DRVB0557-KNR-AV-TYP-VM      PIC  9(004).
           05  PO-DRVB0557-AV-DAT-VON-VM      PIC  X(010).
           05  PO-DRVB0557-KNR-AV-TYP-VB      PIC  9(004).
           05  PO-DRVB0557-AV-DAT-VON-VB      PIC  X(010).

      ******** ENDE COPY-MEMBER ************************************

