      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.08.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2261.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2261.CPv  $
      *
      *   Rev 2.1   28 Oct 2008 15:55:40   t13ex27
      *AKI00743: DTA-Kontrolle Teil 3
      *
      *   Rev 2.0   14 Aug 2008 11:28:54   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER FÜR DIE MASCHINELLE BESTANDSKONTROLLE         *
      *        - GDGfile                                             *
      *          SYSINMEMBER FUER information, welche gdg's von      *
      *          der jcl verarbeitet werden soll                     *
      *          KEY-PRIM-PRODUKTID wird für das Setzen des          *
      *          GDG-Status nach der Verarbeitung benötigt           *
      *    ANMERKUNG:                                                *
      *                                                              *
      *                                                              *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00743!14.08.08!2.1 ! NEU                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI2261.

           05  GDGDATEI.
               10  GDGNAME       PIC X(35).
               10  GDGGEN        PIC X(08).
           05  JOBNAME           PIC X(08).
           05  KEY-PRIM-PRODUKTID.
               10  GESELLSCHAFT         PIC 9(02).
               10  U-NR                 PIC 9(02).
               10  DATENID              PIC 9(02).
      ***************************************************
