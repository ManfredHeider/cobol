      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2569.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2569.CPv  $
      *
      *   Rev 2.2   Feb 28 2013 18:11:10   t13ex27
      *FKI00911: Verträge löschen-Löschen KVB
      *
      *   Rev 2.1   Sep 18 2012 17:45:06   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-Ticketver
      *arbeitung
      *
      *   Rev 2.0   Mar 02 2012 10:27:18   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00886!02.03.12! 2.1! NEU                       !I. Frenzel*
      * FKI00911!25.02.13! 2.2! Schalter für Delete einge-!I. Frenzel*
      *         !        !    ! fügt                      !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRKI2569.
           05  PI-DATEN.

               10  PI-DRKI2569-KVBVBPERSONENID          PIC X(013).
               10  PI-DRKI2569-KVBKIPERSONENID          PIC X(013).

       01  PO-DRKI2569.
           05  PO-DRKI2569-RC               PIC  X(003).
               88 PO-DRKI2569-RC-OK              VALUE 'OK '.
               88 PO-DRKI2569-RC-NOK             VALUE 'NOK'.

250213     05  PO-DRKI2569-DATEN            PIC  X(001).
250213         88 PO-DRKI2569-KVB-DEL-J          VALUE 'J'.
250213         88 PO-DRKI2569-KVB-DEL-N          VALUE 'N'.

