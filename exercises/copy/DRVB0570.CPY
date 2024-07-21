      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0570.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRVB0570.CPv  $
      *
      *   Rev 2.1   Aug 09 2002 11:04:48   t13ae27
      *EMAIL/URL-Einführung
      *
      *   Rev 2.0   15 May 2001 13:33:48   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0570, ALLGEMEINE SCHNITTSTELLE           *
      *                 FÜR CTV-BATCH UPRO-CALL                      *
      *    --------------------------------------------------------  *
      *    ERSTELLT: P. Dürr                                         *
      *    DATUM:    27.02.2002                                      *
      ****************************************************************
      *    VERSION   1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00587!27.02.02!    ! NEU                       ! P.DÜRR   *
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
      *     PI-DRVB0570-MEMZ-MA-ST-NR
      *
      *   AUSGABE:
      *
      *     PO-DRVB0570-RC
      *     PO-DRVB0570-MEMZ-EMAIL       eMail-Adresse
      *     PO-DRVB0570-MEMZ-DAT-VON     Datum-Ab  für eMail-Adresse
      *     PO-DRVB0570-MEMZ-DAT-BIS     Datum-Bis für eMail-Adresse
      *
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRVB0570.
           05  PI-DRVB0570-MA-ST-NR      PIC S9(009) COMP.

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRVB0570.
           05  PO-DRVB0570-RC                 PIC  X(003).
               88  PO-DRVB0570-RC-OK                      VALUE 'OK '.
               88  PO-DRVB0570-RC-NOK                     VALUE 'NOK'.
               88  PO-DRVB0570-RC-NOT-FOUND               VALUE 'NFD'.

           05  PO-DRVB0570-MEMZ-EMAIL         PIC  X(070).
           05  PO-DRVB0570-MEMZ-DAT-VON       PIC  X(026).
           05  PO-DRVB0570-MEMZ-DAT-BIS       PIC  X(010).
           05  PO-DRVB0570-MEMZ-SST-KZ        PIC  X(001).

      ******** ENDE COPY-MEMBER ************************************

