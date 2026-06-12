/**
 * Browser smoke test for the GWAS Opportunity Explorer (issue 010).
 * Runs against the built dist/ via `vite preview`.
 *
 * Checks:
 *  1. Opportunity view loads and shows rows.
 *  2. Country filter narrows rows.
 *  3. Clicking a row opens the country story.
 *  4. The selected condition is highlighted in the condition table.
 *  5. Summary panel values are non-empty.
 *  6. URL contains country and condition params after navigation.
 *  7. Reloading the URL restores the country story (shareable URL).
 */

import { test, expect } from '@playwright/test'

const BASE = '/GWAS-GBD/'

test.describe('Opportunity view', () => {
  test('loads and shows opportunity rows', async ({ page }) => {
    await page.goto(BASE)
    // Wait for the table to appear
    const table = page.getByTestId('opportunity-table')
    await expect(table).toBeVisible({ timeout: 10000 })
    const rows = page.getByTestId('opportunity-row')
    await expect(rows.first()).toBeVisible()
    const count = await rows.count()
    expect(count).toBeGreaterThan(0)
  })

  test('country filter narrows results', async ({ page }) => {
    await page.goto(BASE)
    await page.getByTestId('opportunity-table').waitFor({ timeout: 10000 })

    const allRows = await page.getByTestId('opportunity-row').count()

    // Select the first country option in the filter
    const countryFilter = page.getByTestId('country-filter')
    await countryFilter.selectOption({ index: 1 })

    const filteredRows = await page.getByTestId('opportunity-row').count()
    expect(filteredRows).toBeLessThan(allRows)
    expect(filteredRows).toBeGreaterThan(0)
  })

  test('condition link filters table to that condition', async ({ page }) => {
    await page.goto(BASE)
    await page.getByTestId('opportunity-table').waitFor({ timeout: 10000 })

    const firstCondition = page.getByTestId('condition-filter-link').first()
    const conditionName = (await firstCondition.innerText()).replace(/\s+/g, ' ').trim()

    await firstCondition.click()

    await expect(page.getByTestId('condition-filter')).toHaveValue(conditionName)
    await expect(page.getByTestId('opportunity-table')).toBeVisible()
    await expect(page.getByTestId('summary-panel')).toHaveCount(0)

    const visibleConditionNames = await page.getByTestId('condition-filter-link').evaluateAll(nodes =>
      nodes.slice(0, 10).map(node => node.textContent.replace(/\s+/g, ' ').trim())
    )
    expect(visibleConditionNames.length).toBeGreaterThan(0)
    expect(visibleConditionNames.every(name => name === conditionName)).toBeTruthy()
  })
})

test.describe('Country story navigation', () => {
  test('clicking a row opens the country story with condition highlighted', async ({ page }) => {
    await page.goto(BASE)
    const firstRow = page.getByTestId('opportunity-row').first()
    await firstRow.waitFor({ timeout: 10000 })

    // Click the first opportunity row
    await firstRow.click()

    // Summary panel should appear
    const summary = page.getByTestId('summary-panel')
    await expect(summary).toBeVisible({ timeout: 8000 })

    // Summary values should be populated (not blank/dash-only)
    const alignmentVal = page.getByTestId('alignment-value')
    await expect(alignmentVal).not.toBeEmpty()
    const text = await alignmentVal.innerText()
    expect(text).not.toBe('—')

    const underShareVal = page.getByTestId('under-attended-share')
    await expect(underShareVal).not.toBeEmpty()

    const underBurdenVal = page.getByTestId('under-attended-burden')
    await expect(underBurdenVal).not.toBeEmpty()

    // Condition table should show at least one highlighted row
    const highlighted = page.locator('[data-highlighted="true"]')
    await expect(highlighted).toBeVisible({ timeout: 5000 })
  })

  test('URL contains country and condition after clicking a row', async ({ page }) => {
    await page.goto(BASE)
    const firstRow = page.getByTestId('opportunity-row').first()
    await firstRow.waitFor({ timeout: 10000 })
    await firstRow.click()
    await page.getByTestId('summary-panel').waitFor({ timeout: 8000 })

    const url = new URL(page.url())
    expect(url.searchParams.has('country')).toBeTruthy()
    expect(url.searchParams.has('condition')).toBeTruthy()
  })

  test('reloading the URL restores the country story (shareable URL)', async ({ page }) => {
    await page.goto(BASE)
    const firstRow = page.getByTestId('opportunity-row').first()
    await firstRow.waitFor({ timeout: 10000 })
    await firstRow.click()
    await page.getByTestId('summary-panel').waitFor({ timeout: 8000 })

    const url = page.url()

    // Reload with the same URL
    await page.goto(url)
    const summary = page.getByTestId('summary-panel')
    await expect(summary).toBeVisible({ timeout: 10000 })

    // Condition should still be highlighted
    const highlighted = page.locator('[data-highlighted="true"]')
    await expect(highlighted).toBeVisible({ timeout: 5000 })
  })
})
