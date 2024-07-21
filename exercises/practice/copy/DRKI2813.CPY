      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2813.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2813.CPv  $
      *
      *   Rev 2.1   Nov 26 2013 11:50:44   t13ex73
      *AKI01261: Bereinigung KSDSen wg. Kundenlöschung - G70
      *
      *   Rev 2.0   Nov 19 2013 09:18:24   t13ex73
      *Archiverstellung
      *
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 8 1 3    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER VERTRAGSLÖSCHUNG AUS DEN KSDS-Dateien        *
      *
      *    SATZLÄNGE   080 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *19.11.13!NEUERSTELLUNG                          !DDY      ! 01  *
      ******************************************************************

      *01  DRKI2813.
      *
           05  'XX'-DATENSTAZ.
               10  FILLER               PIC  X(002).
               10  'XX'-DS-GES          PIC  X(002).
               10  'XX'-DS-VNR          PIC  X(015).
               10  'XX'-DS-VER-ID       PIC  X(013).
               10  'XX'-DS-KND-ID       PIC  X(013).
      *
               10  'XX'-DS-FILLER3      PIC  X(035).
      ******************************************************************
