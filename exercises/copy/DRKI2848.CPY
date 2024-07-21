      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.02.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2848.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2848.CPv  $
      *
      *   Rev 2.1   Mar 24 2014 14:04:18   t13ex73
      *AKI01273: Abgleich von Kundenunterschieden zwischen OKI und 
      *HOST
      *
      *   Rev 2.0   Feb 25 2014 10:53:06   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 8 4 8    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER UNLOAD DER OKITABELLE KUNDE_INTERESSENT      *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   137 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *25.02.14!NEUERSTELLUNG                          !DDY      ! 01  *
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
               10  'XX'-NI-VNAME           PIC  X(001).
                   88 'XX'-VNAME-IST-NULL              VALUE '?'.
               10  'XX'-VNAME              PIC  X(030).
               10  'XX'-NI-NNAME           PIC  X(001).
                   88 'XX'-NNAME-IST-NULL              VALUE '?'.
               10  'XX'-NNAME              PIC  X(030).
               10  'XX'-NI-GEB-DATUM       PIC  X(001).
                   88 'XX'-GEB-DATUM-IST-NULL          VALUE '?'.
               10  'XX'-GEB-DATUM.
                   15 'XX'-GEB-DATUM-TT    PIC  9(002).
                   15 FILLER               PIC  X(001).
                   15 'XX'-GEB-DATUM-MM    PIC  9(002).
                   15 FILLER               PIC  X(001).
                   15 'XX'-GEB-DATUM-JJJJ  PIC  9(004).
               10  'XX'-NI-ANREDE          PIC  X(001).
                   88 'XX'-ANREDE-IST-NULL             VALUE '?'.
               10  'XX'-ANREDE-VZ          PIC  X(001).
               10  'XX'-ANREDE             PIC  9(004).
               10  'XX'-NI-GESCHL          PIC  X(001).
                   88 'XX'-GESCHL-IST-NULL             VALUE '?'.
               10  'XX'-GESCHLECHT         PIC  X(001).
               10  'XX'-ANZ-BETREUER       PIC  9(003).
               10  'XX'-LETZTE-AEND-TS     PIC  X(026).
               10  'XX'-FILLER-2           PIC  X(001).
      *
