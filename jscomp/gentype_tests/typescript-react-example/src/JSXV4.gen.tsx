/* TypeScript file generated from JSXV4.res by genType. */

/* eslint-disable */
/* tslint:disable */

import {make as makeNotChecked} from './hookExample';

import * as React from 'react';

// In case of type error, check the type of 'make' in 'JSXV4.res' and './hookExample'.
export const makeTypeChecked: React.ComponentType<{
  readonly actions?: JSX.Element; 
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: renderMe<any>
}> = makeNotChecked as any;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: unknown = makeTypeChecked as React.ComponentType<{
  readonly actions?: JSX.Element; 
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: renderMe<any>
}> as any;

const JSXV4JS = require('./JSXV4.res.js');

export type CompV4_props<x,y> = { readonly x: x; readonly y: y };

export type person = { readonly name: string; readonly age: number };

export type renderMe<a> = React.ComponentType<{ readonly randomString: string; readonly poly: a }>;

export type props<actions,person,children,renderMe> = {
  readonly actions?: actions; 
  readonly person: person; 
  readonly children: children; 
  readonly renderMe: renderMe
};

export const CompV4_make: React.ComponentType<{ readonly x: string; readonly y: string }> = JSXV4JS.CompV4.make as any;

export type Props = { readonly x: string; readonly y: string };

export const CompV3_make: React.ComponentType<{ readonly x: string; readonly y: string }> = JSXV4JS.CompV3.make as any;

export const CompV3: { make: React.ComponentType<{ readonly x: string; readonly y: string }> } = JSXV4JS.CompV3 as any;

export const CompV4: { make: React.ComponentType<{ readonly x: string; readonly y: string }> } = JSXV4JS.CompV4 as any;
