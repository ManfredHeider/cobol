      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0680.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0680.CPv  $
      *
      *   Rev 2.1   Sep 26 2003 15:51:42   t13ex22
      *AVB00481: Ermittlung frühest möglicher Kündigungstermin
      *
      *   Rev 2.0   Feb 17 2003 11:00:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0680                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0013 (PI/PO)                *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    10.09.2003                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00481!10.09.03! 2.1! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0680                                 *
      *    ZU VB-NR WIRD DAS VB-SEIT DATUM ERMITTELT (DATUM DER
      *    UNTERZEICHNUNG DES VB-VERTRAGES.
      ****************************************************************
      *
       01 PI-DRVB0680.
          05 PI-DRVB0680-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0680-STICHTAG-EUR       PIC X(010).
      *
       01 PO-DRVB0680.

          05 PO-DRVB0680-RC                 PIC  X(003).
             88 PO-DRVB0680-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0680-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0680-DATEN.
             10 PO-DRVB0680-DAUER-JAHRE       PIC  9(002).
      *
      ***** ENDE COPY-MEMBER DRVB0680 *****************************
