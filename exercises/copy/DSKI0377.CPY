      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0377.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DSKI0377.CPv  $
      *
      *   Rev 2.4   Jun 19 2013 18:21:40   t13ex52
      *AKI01236: G10 KFZ - NeuGeschäftTarif
      *
      *   Rev 2.3   28 Sep 2000 10:56:46   T13EX41
      *AKI00065
      *- Umstellung SAVARI auf REOB
      *
      *   Rev 2.2   Jun 14 1999 08:49:10   I13PC48
      *Konstante für SF-Kl. 0 von "21" auf "22" geändert
      *
      *   Rev 2.1   Mar 10 1999 12:07:18   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 03 1999 13:20:54   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   30 Jun 1998 10:26:26   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0377                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    KFZ - Schadensfreiheitsklassen - TCKI0377                 *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!04.08.98!01.01! Neuerstellung            !K-H Müller*
      * --------!--------!-----!--------------------------!----------*
      * FKI00396!13.09.00!     ! SF-Klassen 19 - 25 neu   !C. Heineck*
      *         !        !     ! (GEHT MIT AKI65 IN PROD) !          *
      *         !        !     !                          !          *
      * --------!--------!-----!--------------------------!----------*
      * AKI01236!29.05.13!     ! SF-Klassen 26 - 35 neu   !H.Schlenther*
      *    UE001!        !     !                          !            *
      *         !        !     !                          !            *
      * --------!--------!-----!--------------------------!------------*
      *                                                                *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *                                                                *
      * -------------------------------------------------------------- *
290513*Wie erst jetzt richtig klar geworden ist, handelt es sich bei   *
    >>*den Umsetzungen nicht, wie irrtümlicherweise angenommen, um die *
      *"tatsächlichen SF-Jahre", sondern um einen Index, der in der OKI*
      *genutzt wird um damit auf die Tabelle OKI.KTY_KNR_SCHLUESSEL    *
      *mit dem KTY-Schluessel=44 und dem KNR-Schluessel=Inhalt aus     *
      *SF_KLASSE_KNR(OKI.KFZ_KASKO_EIGEN) zuzugreifen um erst dann die *
      *SF-Jahre zu ermitteln. Die Tabelle OKI.KTY_KNR_SCHLUESSEL liegt *
    <<*in OKI-Hoheit und wird auch nur von der OKI "gepflegt".         *
290513* -------------------------------------------------------------- *
      *
       01  FILLER.
           05 K-SF-IN-JAHREN-M           PIC S9(004) BINARY VALUE  1.
           05 K-SF-IN-JAHREN-S           PIC S9(004) BINARY VALUE  2.
           05 K-SF-IN-JAHREN-1ODER2      PIC S9(004) BINARY VALUE  3.
           05 K-SF-IN-JAHREN-1           PIC S9(004) BINARY VALUE  4.
           05 K-SF-IN-JAHREN-2           PIC S9(004) BINARY VALUE  5.
           05 K-SF-IN-JAHREN-3           PIC S9(004) BINARY VALUE  6.
           05 K-SF-IN-JAHREN-4           PIC S9(004) BINARY VALUE  7.
           05 K-SF-IN-JAHREN-5           PIC S9(004) BINARY VALUE  8.
           05 K-SF-IN-JAHREN-6           PIC S9(004) BINARY VALUE  9.
           05 K-SF-IN-JAHREN-7           PIC S9(004) BINARY VALUE 10.
           05 K-SF-IN-JAHREN-8           PIC S9(004) BINARY VALUE 11.
           05 K-SF-IN-JAHREN-9           PIC S9(004) BINARY VALUE 12.
           05 K-SF-IN-JAHREN-10          PIC S9(004) BINARY VALUE 13.
           05 K-SF-IN-JAHREN-11          PIC S9(004) BINARY VALUE 14.
           05 K-SF-IN-JAHREN-12          PIC S9(004) BINARY VALUE 15.
           05 K-SF-IN-JAHREN-13          PIC S9(004) BINARY VALUE 16.
           05 K-SF-IN-JAHREN-14          PIC S9(004) BINARY VALUE 17.
           05 K-SF-IN-JAHREN-15          PIC S9(004) BINARY VALUE 18.
           05 K-SF-IN-JAHREN-16          PIC S9(004) BINARY VALUE 19.
           05 K-SF-IN-JAHREN-17          PIC S9(004) BINARY VALUE 20.
           05 K-SF-IN-JAHREN-18          PIC S9(004) BINARY VALUE 21.
           05 K-SF-IN-JAHREN-KLASSE0     PIC S9(004) BINARY VALUE 22.
           05 K-SF-NICHT-BEKANNT         PIC S9(004) BINARY VALUE -2.
000913     05 K-SF-IN-JAHREN-19          PIC S9(004) BINARY VALUE 23.
000913     05 K-SF-IN-JAHREN-20          PIC S9(004) BINARY VALUE 24.
000913     05 K-SF-IN-JAHREN-21          PIC S9(004) BINARY VALUE 25.
000913     05 K-SF-IN-JAHREN-22          PIC S9(004) BINARY VALUE 26.
000913     05 K-SF-IN-JAHREN-23          PIC S9(004) BINARY VALUE 27.
000913     05 K-SF-IN-JAHREN-24          PIC S9(004) BINARY VALUE 28.
000913     05 K-SF-IN-JAHREN-25          PIC S9(004) BINARY VALUE 29.
      * *** Beginn Aenderung 29. 05.2013 **************
290513     05 K-SF-IN-JAHREN-26          PIC S9(004) BINARY VALUE 30.
290513     05 K-SF-IN-JAHREN-27          PIC S9(004) BINARY VALUE 31.
290513     05 K-SF-IN-JAHREN-28          PIC S9(004) BINARY VALUE 32.
290513     05 K-SF-IN-JAHREN-29          PIC S9(004) BINARY VALUE 33.
290513     05 K-SF-IN-JAHREN-30          PIC S9(004) BINARY VALUE 34.
290513     05 K-SF-IN-JAHREN-31          PIC S9(004) BINARY VALUE 35.
290513     05 K-SF-IN-JAHREN-32          PIC S9(004) BINARY VALUE 36.
290513     05 K-SF-IN-JAHREN-33          PIC S9(004) BINARY VALUE 37.
290513     05 K-SF-IN-JAHREN-34          PIC S9(004) BINARY VALUE 38.
290513     05 K-SF-IN-JAHREN-35          PIC S9(004) BINARY VALUE 39.
      * *** Ende Aenderung 29. 05.2013 **************
      *                                                                *
      ******************************************************************
      *          E n d e    Copy-Buch   DSKI0377                       *
      ******************************************************************
