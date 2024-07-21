      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0468.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0468.CPv  $
      *
      *   Rev 2.2   28 Sep 2000 11:07:24   T13EX44
      *NEU: KNR-ZAHLWEISE
      *     FAELLIG-VON
      *     S-ERSTE-MELDUNG-ZU-
      *     SPARPLAN
      *
      *   Rev 2.1   Jun 02 1999 13:51:42   T13EX44
      * 
      *
      *   Rev 2.0   May 26 1999 16:57:40   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:40:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0468  FUNKTION DRPA0468 F?R UPPA0061     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Parent                                          *
      *    DATUM:    11.11.9999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.11.97! 01 ! NEUERSTELLUNG             ! PARENT   *
      *         !        !    !                           !          *
      * AFPA0012!03.12.97!    ! ÄNDERUNGSKENNZEICHEN      ! WOLFF    *
      *         !        !    ! RAUSGENOMMEN;             !          *
      *         !        !    ! NEU: PVPT                 !          *
      * APA00105!25.09.00!2.02! NEU: KNR-ZAHLWEISE        ! SCHEIDEL *
      *         !        !    !      FAELLIG-VON          !          *
      *         !        !    !      S-ERSTE-MELDUNG-ZU-  !          *
      *         !        !    !      SPARPLAN             !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA0468.
          05 PI-DRPA0468-WAEHRUNG           PIC S9(004) BINARY.
250900    05 PI-DRPA0468-KNR-ZAHLWEISE      PIC S9(004) BINARY.
250900    05 PI-DRPA0468-FAELLIG-VON        PIC  X(010).
          05 PI-DRPA0468-PMPP.
             COPY DRPA0297.
          05 PI-DRPA0468-PVPT.
             COPY DRPA0410.
       01 PO-DRPA0468.
          05 PO-DRPA0468-RC                 PIC  X(003).
             88 PO-DRPA0468-RC-OK                VALUE 'OK '.
             88 PO-DRPA0468-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
031297*   05 PO-DRPA0468-AKZ                PIC  X(003).
031297*      88 PO-DRPA0468-KZ-KEINE-AENDERUNG    VALUE 'KAE'.
031297*      88 PO-DRPA0468-KZ-AEND-MIT-HIST      VALUE 'AMH'.
031297*      88 PO-DRPA0468-KZ-AEND-OHNE-HIST     VALUE 'AOH'.
031297    05 PO-DRPA0468-DATEN.
250900       10 PO-DRPA0468-S-ERSTE-MELDUNG PIC  9(001).
250900        88 ERSTE-MELDUNG-ZU-SPARPLAN  VALUE 1.
250900        88 UNDEFINIERTE-MELDUNG       VALUE 0.
031297       10 PO-DRPA0468-PVPT.
031297          COPY DRPA0410.
      *
      ****** Ende Copy-Member ****************************************
