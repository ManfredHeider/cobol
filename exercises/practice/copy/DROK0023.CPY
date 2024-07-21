      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0023.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DROK0023.CPv  $
      *
      *   Rev 2.3   May 14 2007 10:57:56   t13ex48
      *AKI00557: Ticketverfahren OKI: RUECK-DTA Teil 2
      *
      *   Rev 2.2   27 Mar 2007 10:19:38   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.1   Feb 27 2007 12:56:30   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.0   Apr 25 2006 12:48:26   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRok0023                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRok0023                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  25.04.2006/14:14:28
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!10.03.06! 2.1! NEU                       !I. Frenzel*
      * -------------------------------------------------------------*
250407* AKI00557!25.04.07! 2.2! Neues Attribut f. Update  !H.Sebbach *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRok0023
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRok0023.
           05  PI-DRok0023-KEY.


             10 PI-DRok0023-HADTICKETID     PIC S9(18) COMP-3.


           05  PI-DRok0023-DATEN.


             10 PI-DRok0023-HADGEAENDERTAM   PIC  X(10).
250407       10 PI-DRok0023-HADSTATUS        PIC S9(04) COMP.


      *
       01  PO-DRok0023.
           05  PO-DRok0023-RC                 PIC  X(003).
               88 PO-DRok0023-RC-OK                        VALUE 'OK '.
               88 PO-DRok0023-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
