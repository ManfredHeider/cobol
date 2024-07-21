      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1237.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1237.CPv  $
      *
      *   Rev 2.1   May 02 2006 09:07:00   t13ex39
      *AKI00215: Umstellung aller Fremdverträge aud EURO
      *
      *   Rev 2.0   05 Jul 2001 17:06:34   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1237                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1237                    *
      *    --------------------------------------------------------   *
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00098!13.03.00!1.0 ! NEU                       ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRKI1237.
           05  PI-UPKI1237-DATEN.
               15 PI-DRKI1237-NI              PIC S9(4) COMP.
               15 PI-DRKI1237-BETRAG          PIC 9(11)V99.
               15 PI-DRKI1237-WKZ             PIC X(01).
               15 PI-DRKI1237-VERT-ID         PIC X(13).
               15 PI-DRKI1237-PERSONENID      PIC X(13).
               15 PI-DRKI1237-LKZ             PIC X(02).


       01  PO-DRKI1237.
           05  PO-UPKI1237-DATEN.
               15 PO-DRKI1237-BETRAG          PIC 9(11)V99.
               15 PO-DRKI1237-WKZ             PIC X(01).
               15 PO-DRKI1237-AEND-KZ         PIC X(01).
                  88 PO-DRKI1237-SATZ-GEAEND       VALUE 'J'.
                  88 PO-DRKI1237-SATZ-NICHT-GEAEND VALUE ' '.

