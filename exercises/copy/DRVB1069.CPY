      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.03.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1069.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1069.CPv  $
      *
      *   Rev 2.1   Apr 05 2013 15:17:40   t13ae30
      *AAS01065: Brieferstellung
      *
      *   Rev 2.0   Mar 07 2013 16:50:24   t13ae30
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1069, ALLGEMEINE SCHNITTSTELLE           *
      *                 FÜR CTV-BATCH UPRO-CALL                      *
      *    --------------------------------------------------------  *
      *    ERSTELLT: TH.WILLIGT                                      *
      *    DATUM:    07.03.2013                                      *
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS01069!07.03.13!    ! NEU                       ! TH.WILLIG*
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB1069.
           05  PI-DRVB1069-ST-NR              PIC S9(009) COMP.
           05  PI-DRVB1069-DATUM-EUR          PIC  X(010).

       01  PO-DRVB1069.
           05  PO-DRVB1069-RC                 PIC  X(003).
               88  PO-DRVB1069-RC-OK                      VALUE 'OK '.
               88  PO-DRVB1069-RC-NOK                     VALUE 'NOK'.
               88  PO-DRVB1069-RC-NOT-FOUND               VALUE 'NFD'.

           05  PO-DRVB1069-TBAU0003           PIC  X(4000).

      ******** ENDE COPY-MEMBER **************************************
