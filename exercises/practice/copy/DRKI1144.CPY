      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1144.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRKI1144.CPv  $
      *
      *   Rev 2.1   12 Nov 2001 10:55:24   t13ex27
      *AKI00181 - Produktionsübergabe
      *
      *   Rev 2.0   07 Jun 2000 17:08:32   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    SUPPORT                                                     *
      *    COPY-MEMBER FÜR DIE MASCHINELLE BESTANDSKONTROLLE           *
      *        - GDGfile                                               *
      *          SYSINMEMBER FUER information, welche gdg's von        *
      *          der jcl verarbeitet werden soll                       *
      *          KEY-PRIM-PRODUKTID wird für das Setzen des GDG-Status *
      *          nach der Verarbeitung benötigt                        *
      *    ANMERKUNG:                                                  *
      *                                                                *
      *                                                                *
      ******************************************************************
      * AENDERUNGEN:                                                    *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME        *
      *-----------------------------------------------------------------*
      * AKI00177!15.11.00!1.0 ! NEUERSTELLUNG             !SPOOREN      *
      *         !        !    !                           !FRENZEL      *
      *         !        !    !                           !             *
      *         !        !    !                           !             *
      *         !        !    !                           !             *
      *         !        !    !                           !             *
      * ENDE-LOGBUCH                                                    *
      *******************************************************************
CCC009*    SKIP1
       01  DRKI1144.
           05  GDGNAME           PIC X(25).
           05  JOBNAME           PIC X(08).
           05  KEY-PRIM-PRODUKTID.
               10  GESELLSCHAFT         PIC 9(02).
               10  U-NR                 PIC 9(02).
               10  DATENID              PIC 9(02).
               10  NUMMER               PIC X(08).
      *******************************************************************
