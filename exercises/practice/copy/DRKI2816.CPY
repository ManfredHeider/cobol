      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.12.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2816.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2816.CPv  $
      *
      *   Rev 2.1   Mar 24 2014 14:04:12   t13ex73
      *AKI01273: Abgleich von Kundenunterschieden zwischen OKI und 
      *HOST
      *
      *   Rev 2.0   Dec 03 2013 08:49:56   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 8 1 6    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER UNLOAD DER HOSTTABELLE KUNDE                 *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   160 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *03.12.13!NEUERSTELLUNG                          !DDY      ! 01  *
      *AKI01273!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
      *01  DRKI2813.
      *
           05  'XX'-DATENSTAZ.
               10  'XX'-PERSONENID         PIC  X(026).
               10  'XX'-STATUS             PIC  X(001).
                   88 'XX'-IST-ALTKUNDE                VALUE 'A'.
                   88 'XX'-IST-KUNDE                   VALUE 'K'.
               10  'XX'-VNAME              PIC  X(030).
               10  'XX'-NI-VNAME           PIC  X(001).
                   88 'XX'-VNAME-IS-NULL               VALUE '?'.
               10  'XX'-NNAME              PIC  X(030).
               10  'XX'-SUCH-VNAME         PIC  X(030).
               10  'XX'-NI-SUCH-VNAME      PIC  x(001).
                   88 'XX'-SUCH-VNAME-IS-NULL          VALUE '?'.
               10  'XX'-SUCH-NNAME         PIC  X(030).
               10  'XX'-GEB-DATUM.
                   15 'XX'-GEB-DATUM-TT    PIC  X(002).
                   15 FILLER               PIC  X(001).
                   15 'XX'-GEB-DATUM-MM    PIC  X(002).
                   15 FILLER               PIC  X(001).
                   15 'XX'-GEB-DATUM-JJJJ  PIC  X(004).
               10  'XX'-NI-GEB-DATUM       PIC  X(001).
                   88 'XX'-GEB-DATUM-IS-NULL           VALUE '?'.
               10  'XX'-ANREDE             PIC S9(004) COMP.
               10  'XX'-GESCHLECHT         PIC  X(001).
               10  'XX'-ANZ-BETREUER       PIC  9(004) COMP.
               10  'XX'-LETZTE-AEND-TS     PIC  X(026).
               10  'XX'-FILLER-1           PIC  X(001).
      *
