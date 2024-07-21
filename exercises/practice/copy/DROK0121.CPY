      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0121.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0121.CPv  $
      *
      *   Rev 2.1   May 29 2012 10:21:36   t13ex27
      *AOK00047: Auswertung Einarbeitungszeiten und Satzstatistik-D
      *B und OZPs
      *
      *   Rev 2.0   Mar 08 2012 15:46:28   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AOK00047!08.03.12! 2.1! NEU                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DROK0121.

           05  FILLER                                  PIC  X(013).

       01  PO-DROK0121.
           05  PO-DROK0121-RC                          PIC  X(003).
               88 PO-DROK0121-RC-OK                        VALUE 'OK '.
               88 PO-DROK0121-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DROK0121-DATEN.

             10 PO-DROK0121-OKEJAHR                PIC S9(4)  COMP.
             10 PO-DROK0121-OKEMONAT               PIC S9(4)  COMP.

      ********************** Datenende *********************************
