      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0749.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0749.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:58:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 13:43:36   T13EX47
      *SELECT fuer ZWKI0357
      *
      *   Rev 1.0   06 Oct 1998 15:35:56   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRki0749  FUNKTION lese-OZP                  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: gbe                                             *
      *    DATUM:    28.10.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!28.10.98! 01 ! NEUERSTELLUNG             !   gbe    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRKI0749.
           05  PI-DRKI0749-VNR                PIC X(15).
           05  PI-DRKI0749-PGEB-GES           PIC S9(4) BINARY.

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRKI0749.
           05  PO-DRKI0749-RC                  PIC  X(03).
               88  PO-DRKI0749-RC-OK           VALUE 'OK '.
               88  PO-DRKI0749-RC-NFD          VALUE 'NFD'.
           05 PO-DRKI0749-NI-TAB.
             10 PO-DRKI0749-NI-ELE PIC S9(04) COMP OCCURS  2 TIMES.
           05 PO-DRKI0749-NIS  REDEFINES  PO-DRKI0749-NI-TAB.
291098        10 PO-DRKI0749-NI-VERTRAGSBEGINN  PIC  S9(004) COMP.
291098        10 PO-DRKI0749-NI-DATVERTRAEND    PIC  S9(004) COMP.
           05 PO-DRKI0749-MITA-DATEN.
              10 PO-DRKI0749-VERTRAGS-ID        PIC  X(13).
              10 PO-DRKI0749-VERTRAGSBEGINN     PIC  X(10).
              10 PO-DRKI0749-DATVERTRAEND       PIC  X(10).
      *
      ******** ENDE COPY-MEMBER (DRKI0749) *******************

